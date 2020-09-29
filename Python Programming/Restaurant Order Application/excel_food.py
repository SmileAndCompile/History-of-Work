import openpyxl
from openpyxl.styles import Font
import sqlite3
from datetime import datetime

# inspections database
connection = sqlite3.connect("inspections.db");
cursor = connection.cursor();

# create the new excel workbook / Violation Types sheet
wb = openpyxl.Workbook();
sheet = wb.active;
sheet.title = "Violation Types";

bold11font = Font(name='Calibri', size = 11, bold = True);

# set the names of the header cells
sheet['A1'] = "Code";
sheet['B1'] = "Description";
sheet['C1'] = "Count";

# set the dimensions of the cells
sheet.column_dimensions['A'].width = 15;
sheet.column_dimensions['B'].width = 45;
sheet.column_dimensions['C'].width = 15;

# set the header cells to bold font
sheet['A1'].font = bold11font;
sheet['B1'].font = bold11font;
sheet['C1'].font = bold11font;

# query the database (return the code, description and amount of violations for each violation_code)
data = cursor.execute("""SELECT violation_code,
						 violation_description,
						 COUNT(violation_code) as Violations
				  FROM Violation
				  GROUP BY violation_code
				  ORDER BY violation_code ASC""");

# set initial index value (we will fill the excel spreadsheet with the relevant data)
index = 2;

# write the data to the ViolationTypes spreadsheet
for row in data:
	sheet['A' + str(index)] = row[0];
	sheet['B' + str(index)] = row[1];
	sheet['C' + str(index)] = row[2];
	index += 1;

# retrieve the max row of the ViolationTypes spreadsheet
maxRow = sheet.max_row;

# set / calcuate the cells with the number of Total Violations
sheet['B' + str(maxRow + 1)] = "Total Violations";
sheet['B' + str(maxRow + 1)].font = bold11font;
sumIndex = 'C' + str(maxRow);
sheet['C' + str(maxRow + 1)] = "=SUM(C2:{})".format(sumIndex);

# save the excel workbook
wb.save("ViolationTypes.xlsx");


