#!/bin/bash

# all secured files are within the /Secure directory

if [[ $1 = "-c" ]]
then
	echo "Creating clean state. File created: clean.txt. Call ./IDS.sh using option -o to verify current clean state"
	> clean.txt # clear clean file (as we are creating a new one)

	# record of files / directories contained in clean state
	contents=(`ls Secure`)
	echo -n "record: " >> clean.txt
	for((i=0;i<${#contents[@]};i++)); do
		echo -n " ${contents[i]}" >> clean.txt
	done

	echo "" >> clean.txt

	# collect permissions of each file / directory in array
	permissions=(`ls -l Secure | awk '{if(NR>1)print}' | awk '{print $1}'`)
	filenames=(`ls -l Secure | awk '{if(NR>1)print}' | awk '{print $9}'`)

	# output each filename hash value and permission hash value to clean state
	# p: is added to the front of filename pre-hash - to act as the identifier for file permissions
	for ((i=0;i<${#permissions[@]};i++)); do
		echo -n "p:${filenames[i]}" | openssl sha1 | awk '{print $2}' | tr -d '\n' >> clean.txt
		echo -n " " >> clean.txt
		echo -n "${permissions[i]}" | openssl sha1 | awk '{print $2}' | tr -d '\n' >> clean.txt
		echo "" >> clean.txt
	done

	# record of modifiers for each file (i.e date and time last modified, owner, file size, group)
	modifiers=$(ls -l Secure | awk '{if(NR>1)print}')

	# output each filename hash value and the modifers hash value to clean state
	# m: is added to the front of filename pre-hash - to act as the identifier for file modifiers
	f=0
	while read -r line; do
		cut1=${line#* } # remove permissions parameter
		cut2=${cut1% *} # remove file name (result = file modifiers without permissions)

		echo -n "m:${filenames[f]}" | openssl sha1 | awk '{print $2}' | tr -d '\n' >> clean.txt
		echo -n " " >> clean.txt
		echo -n "$cut2" | openssl sha1 | awk '{print $2}' | tr -d '\n' >> clean.txt
		echo "" >> clean.txt
		((f++))
	done <<< "$modifiers"

	# output each filename hash value and contents of file hash value to clean state. c: is added to front of filename pre-hash - to act as the identifier for file contents
	for ((i=0;i<${#filenames[@]};i++)); do
		first=$( printf "%1.1s" "${permissions[i]}" ) # first character of permissons (determines if file is a file or directory)
		if [ $first != "d" ] # skip directories (we only want to hash file contents)
		then
			echo -n "c:${filenames[i]}" | openssl sha1 | awk '{print $2}' | tr -d '\n' >> clean.txt
			echo -n " " >> clean.txt
			echo -n "${permissions[i]}" | awk '{print $2}' | tr -d '\n' >> clean.txt	
			echo "" >> clean.txt
		fi
	done	

elif [[ $1 = "-o" ]]
then
	echo "Verifying clean state."

	permissionchanges="" # list of any files / directories that have had their permissions changed since clean state
	changed="" # list of any files that have changed since clean state (includes file / directory modifier changes and file content changes)
	created="" # list of any files that have been created since clean state
	deleted="" # list of any files that have beed deleted since clean state

	# record of files / directories contained in verification state
	validate=`ls Secure` # record of files / directories in validation state

 	# record of files / directories in clean state (check removes record: identifier)
	records=$(grep 'record: ' clean.txt)
	check=${records#*record: } # removes record: identifier
	
	for c in $check; do
		if [[ "$validate" != *"$c"* ]]; then
			# file in clean state is not in validation state, therefore the file has been deleted from validation state)
			deleted="$deleted $c"
		fi
	done	

	for v in $validate; do
		if [[ "$check" != *"$v"* ]]; then
			# file in validation state not in clean state, therefore the file has been created / added to validation state
			created="$created $v"
		fi
	done

	# collect permissions of each file / directory in array
	permissions=(`ls -l Secure | awk '{if(NR>1)print}' | awk '{print $1}'`)
	filenames=(`ls -l Secure | awk '{if(NR>1)print}' | awk '{print $9}'`)

	# retrieve each filename and permissions from working directory to compare to clean state
	# p: is added to the front of filename pre-hash to identify permission hash values for specific files
	for ((i=0;i<${#permissions[@]};i++)); do
		identifier=`echo -n "p:${filenames[i]}" | openssl sha1 | awk '{print $2}' | tr -d '\n'`
		permissionhash=`echo -n "${permissions[i]}" | openssl sha1 | awk '{print $2}' | tr -d '\n'`
		fullhash="$identifier $permissionhash"

		count=`grep -c "$identifier" clean.txt`
		result=`grep -c "$fullhash" clean.txt`
		if [ $count == 1 ] && [ $result != 1 ]; 
		then
			# if the identifier is found but the full hash does not match, then permissions for the file have changed
			permissionchanges="$permissionchanges ${filenames[i]}"
		fi
	done

	# record of modifiers for each file (i.e date and time last modified, owner, file size, group)
	modifiers=$(ls -l Secure | awk '{if(NR>1)print}')

	# retrieve each filename hash value and the modifers hash value from working directory to compare to clean state
	# m: is added to the front of filename pre-hash - to identify modifier hash values for specific files
	f=0
	while read -r line; do

		cut1=${line#* } # remove permissions parameter
		cut2=${cut1% *} # remove file name (result = file modifiers without permissions)
		identifier=`echo -n "m:${filenames[f]}" | openssl sha1 | awk '{print $2}' | tr -d '\n'`
		modifierhash=`echo -n "$cut2" | openssl sha1 | awk '{print $2}' | tr -d '\n'`
		fullhash="$identifier $modifierhash"

		count=`grep -c "$identifier" clean.txt`
		result=`grep -c "$fullhash" clean.txt`
		if [ $count == 1 ] && [ $result != 1 ];
		then
			# if the identifier is found but the full hash does not match, then the file has been changed
			changed="$changed ${filenames[f]}"
		fi
		((f++))
	done <<< "$modifiers"



	# retrieve each filename hash value and contents of file hash value from working directory to compare to clean state. c: is added to front of filename pre-hash to identify file contents hash values for specific files
	for ((i=0;i<${#filenames[@]};i++)); do
		first=$( printf "%1.1s" "${permissions[i]}" ) # first character of permissons (determines if file is a file or directory)
		if [ $first != "d" ] # skip directories (we only want to hash file contents)
		then
			
			identifier=`echo -n "c:${filenames[i]}" | openssl sha1 | awk '{print $2}' | tr -d '\n'`
			contentshash=`openssl sha1 Secure/${filenames[i]} | awk '{print $2}' | tr -d '\n'`
			
			fullhash="$identifier $contenthash"

			count=`grep -c "$identifier" clean.txt`
			result=`grep -c "$fullhash" clean.txt`
			if [ $count == 1 ] && [ $result != 1 ]; 	
			then
					# if the identifier is found but the full hash does not match, then the file contents have been changed
				changed="$changed ${filenames[f]}"
			fi
		fi
	done

	if [ -z "$permissionchanges" ] && [ -z "$changed" ] && [ -z "$created" ] && [ -z "$deleted" ]; 
	then
		printf "\n"
		echo "Congratulations, your system is secure"
	else
		printf "\n\n"

		# print out any unauthorized permission changes
		if [ ! -z "$permissionchanges" ];
		then
			echo "Permissions for following files have changed: "
			for i in $permissionchanges; do
				echo -n "$i "
			done
			printf "\n\n"
		fi

		# print out any unauthorized file changes
		if [ ! -z "$changed" ];
		then
			echo "The following files have been changed: "
			for i in $changed; do
				echo -n "$i "
			done
			printf "\n\n"
		fi

		# print out any unauthorized file creation
		if [ ! -z "$created" ];
		then
			echo "The following files have been created: "
			for i in $created; do
				echo -n "$i "
			done
			printf "\n\n"
		fi

		# print out any unauthorized file deleted
		if [ ! -z "$deleted" ];
		then
			echo "The following files have been deleted: "
			for i in $deleted; do
				echo -n "$i "
			done
			printf "\n\n"
		fi
	fi
else
	echo "please run command using -c option to create clean state, or -v option to verify clean state"
fi


