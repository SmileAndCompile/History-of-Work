import sqlite3
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

# task 4 Assignment 2

# connect to the inspections database
connection = sqlite3.connect("inspections.db");
cursor = connection.cursor();

# retrieve the postcode with the highest number of violations
data = cursor.execute("""
		SELECT facility_zip, MAX(Violations)
		FROM
			(
			SELECT facility_zip, COUNT(*) as Violations 
			FROM Inspection
			INNER JOIN Violation ON Inspection.serial_number = Violation.serial_number
			GROUP BY facility_zip
			)
 					 """);

results = data.fetchone();
mostViolationsZip = results[0];

# retrieve the postcode with the least number of violations
data = cursor.execute("""
		SELECT facility_zip, MIN(Violations)
		FROM
			(
			SELECT facility_zip, COUNT(*) as Violations 
			FROM Inspection
			INNER JOIN Violation ON Inspection.serial_number = Violation.serial_number
			GROUP BY facility_zip
			)
 					 """);

results = data.fetchone();
leastViolationsZip = results[0];

# retrieve the violations for each month for the zip with the highest number of violations

# retrieve earliest inspection month / year
data = cursor.execute("""SELECT MIN(activity_date) FROM Inspection""");
earliestDate = data.fetchone();
earliestYear = int(earliestDate[0].split('-')[0]);
earliestMonth = int(earliestDate[0].split('-')[1]);

# retrieve latest inspection month / year
data = cursor.execute("""SELECT MAX(activity_date) FROM Inspection""");
latestDate = data.fetchone();
latestYear = int(latestDate[0].split('-')[0]);
latestMonth = int(latestDate[0].split('-')[1]);

year = earliestYear;
month = earliestMonth;

# array of all the dates (month / year) between the earliest inspection month / year and the latest inspection month / year
dates = [];

# configure array of dates from earliest to latest date
while ((year <= latestYear) & (not ((month > latestMonth) & (year == latestYear)))):
	dates.append("%s-%s" % (month, year));
	month += 1;
	if (month >= 13):
		month = 1;
		year += 1;

# retrieve the data for each month for the postcode with highest number of violations
mostMonthlyViolations = [];
for date in dates:
	month = int(date.split('-')[0]);
	year = int(date.split('-')[1]);
	if (month < 10):
		dateString = '%s-0%s%%' % (year, month);
	else:
		dateString = '%s-%s%%' % (year, month);

	data = cursor.execute("""	
			SELECT COUNT(*) 
			FROM Inspection
			INNER JOIN Violation ON Inspection.serial_number = Violation.serial_number
			WHERE Inspection.activity_date LIKE ? AND Inspection.facility_zip = ?""", [dateString, mostViolationsZip]);
	results = data.fetchone();
	mostMonthlyViolations.append(results[0]);

# retrieve the data for each month for the postcode with lowest number of violations
leastMonthlyViolations = [];
for date in dates:
	month = int(date.split('-')[0]);
	year = int(date.split('-')[1]);
	if (month < 10):
		dateString = '%s-0%s%%' % (year, month);
	else:
		dateString = '%s-%s%%' % (year, month);

	data = cursor.execute("""	
			SELECT COUNT(*) 
			FROM Inspection
			INNER JOIN Violation ON Inspection.serial_number = Violation.serial_number
			WHERE Inspection.activity_date LIKE ? AND Inspection.facility_zip = ?""", [dateString, leastViolationsZip]);
	results = data.fetchone();
	leastMonthlyViolations.append(results[0]);

# list containing the total violations per month for all of california
totalMonthlyViolations = [];

# list containing the number of businesses that were inspected each month for all of california
monthlyZipcodesInspected = [];

for date in dates:
	month = int(date.split('-')[0]);
	year = int(date.split('-')[1]);
	if (month < 10):
		dateString = '%s-0%s%%' % (year, month);
	else:
		dateString = '%s-%s%%' % (year, month);

	# retrieve the total number of violations for california for each month
	data = cursor.execute("""
			SELECT COUNT(*) as Violations
			FROM Inspection
			INNER JOIN Violation ON Inspection.serial_number = Violation.serial_number
			WHERE Inspection.activity_date LIKE ? AND Inspection.facility_state LIKE '%CA%'
			""", [dateString]);

	# total violations for each month
	total = data.fetchone();

	# add the monthly violations to our list of monthly totals
	totalMonthlyViolations.append(total[0]);

	# retrieve the number of distinct zip codes that were inspected in california for each month
	data = cursor.execute("""
		SELECT COUNT(*)
		FROM 
			(
			SELECT DISTINCT Inspection.facility_zip
			FROM Inspection
			WHERE Inspection.activity_date LIKE ? AND Inspection.facility_state LIKE '%CA%'
			)""", [dateString]);

	# number of distinct postcodes which were inspected for each month
	zipCodes = data.fetchone();

	# append the amount of distinct zip codes for the month to our monthly list of zip codes
	monthlyZipcodesInspected.append(zipCodes[0]);

# create numpy arrays for calculating the monthly average
npTotalMonthlyViolations = np.array(totalMonthlyViolations);
npMonthlyZipcodesInspected = np.array(monthlyZipcodesInspected);

# retrieve the length of dates (the amount of months present in the data)
datesLength = len(dates);

# use numPy to calculate the average monthly Violations for all of California
averageMonthlyViolations = np.divide(npTotalMonthlyViolations, npMonthlyZipcodesInspected, out=np.zeros(datesLength),where=(npMonthlyZipcodesInspected!=0));

# plot the monthly violations for the zip codes with the most / least number of violations, and the average monthly violations for california
plt.plot(dates, mostMonthlyViolations, "--b", dates, leastMonthlyViolations, "--r", dates, averageMonthlyViolations, "--g");
plt.xlabel("Date");
plt.ylabel("Violations");
plt.title("Monthly Violations for Zip Code with the Most / Least Total Violations");
plt.xticks(rotation=70, size=6);

# prepare legend
mostViolations = mpatches.Patch(color="blue", label="Most Violations");
leastViolations = mpatches.Patch(color="red", label="Least Violations");
averageViolations = mpatches.Patch(color="green", label="Average Violations");
plt.legend(handles=[mostViolations, leastViolations, averageViolations], framealpha=0.25);

# show the plot
plt.show();

# clear the plot
plt.clf();

# list for the total number of monthly Violations for McDonalds
mcdTotalMonthlyViolations = []

# list for the total number of distinct McDonalds Restaurants that were inspected each month
mcdTotalMonthlyBusinesses = []

# retrieve the total monthly violations for each mcdonalds
for date in dates:
	month = int(date.split('-')[0]);
	year = int(date.split('-')[1]);
	if (month < 10):
		dateString = '%s-0%s%%' % (year, month);
	else:
		dateString = '%s-%s%%' % (year, month);

	# retrieve the total number of violations for all McDonalds each month
	data = cursor.execute("""	
			SELECT COUNT(*) 
			FROM Inspection
			INNER JOIN Violation ON Inspection.serial_number = Violation.serial_number
			WHERE Inspection.activity_date LIKE ? AND Inspection.facility_name LIKE '%MCDONALD%'""", [dateString]);
	violations = data.fetchone();

	# append the total violations for the month to our list of monthly Violations
	mcdTotalMonthlyViolations.append(violations[0]);

	# retrieve the number of distinct mcdonalds that were inspected for each month
	data = cursor.execute("""	
		SELECT COUNT(*) 
		FROM 
			(
			SELECT DISTINCT facility_name
			FROM Inspection
			WHERE (activity_date LIKE ?) AND (facility_name LIKE '%MCDONALD%')
			)""", [dateString]);
	mcdonaldsNum = data.fetchone();

	# append the number of distinct McDonalds Restaurants inspected for the month
	mcdTotalMonthlyBusinesses.append(mcdonaldsNum[0]);

# create numpy arrays for calculating the monthly averages
npMcdTotalMonthlyViolations = np.array(mcdTotalMonthlyViolations);
npMcdTotalMonthlyBusinesses = np.array(mcdTotalMonthlyBusinesses);

# use numPy to calculate the average monthly Violations for all McDonalds Restaurants
mcdMonthlyViolations = np.divide(npMcdTotalMonthlyViolations, npMcdTotalMonthlyBusinesses, out=np.zeros(datesLength),where=(npMcdTotalMonthlyBusinesses!=0));

# list for the total number of monthly Violations for Burger King
burgerKingTotalMonthlyViolations = []

# list for the total number of distinct Burger King Restaurants that were inspected each month
burgerKingTotalMonthlyBusinesses = []

# retrieve the average monthly violations for each burger king
for date in dates:
	month = int(date.split('-')[0]);
	year = int(date.split('-')[1]);
	if (month < 10):
		dateString = '%s-0%s%%' % (year, month);
	else:
		dateString = '%s-%s%%' % (year, month);

	# retrieve the total number of violations for all burger kings for each month
	data = cursor.execute("""	
			SELECT COUNT(*) 
			FROM Inspection
			INNER JOIN Violation ON Inspection.serial_number = Violation.serial_number
			WHERE Inspection.activity_date LIKE ? AND Inspection.facility_name LIKE '%BURGER KING%'""", [dateString]);
	violations = data.fetchone();

	# append the total violations for the month to our list of monthly Violations
	burgerKingTotalMonthlyViolations.append(violations[0]);

	# retrieve the number of distinct Burger Kings that were inspected for each month
	data = cursor.execute("""	
		SELECT COUNT(*) 
		FROM 
			(
			SELECT DISTINCT facility_name
			FROM Inspection
			WHERE activity_date LIKE ? AND facility_name LIKE '%BURGER KING%'
			)""", [dateString]);
	burgerKingNum = data.fetchone();

	# append the number of distinct Burger King Restaurants inspected for the month
	burgerKingTotalMonthlyBusinesses.append(burgerKingNum[0]);

# create numpy arrays for calculating the monthly averages
npBurgerKingTotalMonthlyViolations = np.array(burgerKingTotalMonthlyViolations);
npBurgerKingTotalMonthlyBusinesses = np.array(burgerKingTotalMonthlyBusinesses);
# use numPy to calculate the average monthly Violations for all Burger King Restaurants
burgerKingMonthlyViolations = np.divide(npBurgerKingTotalMonthlyViolations, npBurgerKingTotalMonthlyBusinesses, out=np.zeros(datesLength),where=(npBurgerKingTotalMonthlyBusinesses!=0));

# plot the average monthly violations for mcdonalds / burger kings
plt.plot(dates, mcdMonthlyViolations, "--r", dates, burgerKingMonthlyViolations, "--b");
plt.xlabel("Date");
plt.ylabel("Violations");
plt.title("Average Monthly Violations for McDonalds / Burger King");
plt.xticks(rotation=70, size=6);

# prepare legend
mcdLegend = mpatches.Patch(color="red", label="McDonalds");
bkLegend = mpatches.Patch(color="blue", label="Burger King");
plt.legend(handles=[mcdLegend, bkLegend], framealpha=0.25);

# show the plot
plt.show();

# close the database connection and save the state of the database
connection.commit();
connection.close();

