import sqlite3
from datetime import datetime

# inspections database
connection = sqlite3.connect("inspections.db");
cursor = connection.cursor();

# drop Previous Violations table if it exists
cursor.execute("DROP TABLE IF EXISTS 'Previous Violations'");

# create Previous Violations table
cursor.execute("""CREATE TABLE 'Previous Violations' (
				id INTEGER PRIMARY KEY,
				facility_name TEXT NOT NULL,
				facility_address TEXT NOT NULL,
				facility_zip VARCHAR(15),
				facility_city VARCHAR(25) NOT NULL)""");

#query the database, returning the facility_name of the businesses that have atleast 1 violation - with the results ordered alphabetically. (The name, address, zip and city are entered into the database after the next query where we order the results by the number of violations)
data = cursor.execute("""SELECT Inspection.facility_name,
						 COUNT(Violation.serial_number) as Violations
				  FROM Inspection
				  INNER JOIN Violation ON Violation.serial_number = Inspection.serial_number
				  GROUP BY Inspection.facility_name
				  HAVING Violations >= 1
				  ORDER BY Inspection.facility_name DESC""");

# retrieve the results from the query
sqlResult = data.fetchall();

# print the businesses with atleast 1 violation to console, ordered alphabetically
for row in sqlResult:
	print("Business Name: %s" % (row[0]));

# query the database, returning the facility_name and the number of violations for each distinct facility who have at least 1 violation, with the results ordered by the number of violations (descending order)
data = cursor.execute("""SELECT Inspection.facility_name,
						 COUNT(Violation.serial_number) as Violations,
						 Inspection.facility_address,
						 Inspection.facility_zip,
						 Inspection.facility_city
				  FROM Inspection
				  INNER JOIN Violation ON Violation.serial_number = Inspection.serial_number
				  GROUP BY Inspection.facility_name
				  HAVING Violations >= 1
				  ORDER BY Violations DESC""");

# retrieve the results from the query
sqlResult = data.fetchall();

# determine the longest business name (will be used to format output)
wordlengths = []
for s in sqlResult: 
	wordlengths.append(len(s[0]));

# width of longest business name
width = max(wordlengths) + 1;

# output the results to console
for row in sqlResult:
	diff = width - len(row[0]);

	# fill buffer to format columns
	spaces = ' ' * diff;
	print("Name: %s%s Violations: %s" % (row[0], spaces, str(row[1])));

# insert the businesses with atleast 1 previous violation into the Previous Violations table
data = [];
for row in sqlResult:
	data.append(tuple([row[0], row[2], row[3], row[4]]));

# add all the data to the Previous Violations table
for values in data:
	cursor.execute("""INSERT INTO 'Previous Violations' VALUES (NULL, ?, ?, ?, ?)""", values);

# close the connection to the database and save the state of the database
connection.commit();
connection.close();
