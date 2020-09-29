import os
import logging
import os.path
from os import path
from datetime import datetime
from datetime import timedelta
import json
import csv
import requests
import time
import sys

# process_csv_file reads the data from a csv file, organizing each row of input into an array
# Input Parameters:
#     directory   - subdirectory location that holds the csv file to be processed (usually the spool directory)
#     file_name   - name of the csv file to be processed
#
# Return Parameter:
#     csv_data    - dictionary containing the rows of data from the csv file (1 row from csv file = 1 row in dictionary)
def process_csv_file(directory, file_name):
    
    # dictionary to store csv file data
    csv_data = [];

    # perform error checking on input parameters
    if(not isinstance(directory, str)):
        raise Exception("process_csv_file: input directory must be a String");

    if(not os.path.isdir("./" + directory)):
        raise Exception("process_csv_file: input directory does not exist");

    if(not isinstance(file_name, str)):
        raise Exception("process_csv_file: input file_name must be a String");

    if(not file_name.endswith(".csv")):
        raise Exception("process_csv_file: input file_name must be a .csv file");

    if(not os.path.isfile("./" + directory + "/" + file_name)):
        raise Exception("process_csv_file: input file_name does not exist in the input directory");

    time.sleep(0.1);

    # open file and read input into dictionary
    with open(directory + "/" + file_name, mode='r') as csv_file:
        csv_reader = csv.DictReader(csv_file);

        # add csv data to return dictionary
        for row in csv_reader:
            csv_data.append(row);

        # close csv file
        csv_file.close();
        
    return csv_data;

# process csv_data takes a dictionary of input data, converts it to json and communicates it to a web socket
# Input Parameters:
#    data       - list of dictionaries containing the input data to be communicated to the web socket
#    web_socket - address of the web_socket to communicate the data to
#
# Return Parameter:
#    r          - response from the web socket
def process_csv_data(data, web_socket):

    # perform error checking on input parameters
    if (not isinstance(data, list)):
        raise Exception("process_csv_data: input data must be a list of dictionaries");
    else:
        for i in data:
            if (not isinstance(i, dict)):
                raise Exception("process_csv_data: input data must be a list of dictionaries");

    if(not isinstance(web_socket, str)):
        raise Exception("process_csv_data: web socket must be a String");

    # post request headers (used in request to send sensor data)
    headers = { 'Content-type': 'application/json' };

    #convert sensor data to json
    json_body = json.dumps(data);

    # send data to web socket
    r = requests.post(web_socket, data=json_body, headers=headers);

    return r

def main():

    # name of the file directory that will contain the .csv files containing the sensor data
    fileDirectory = "Sensor Data";

    # web socket url and port to send sensor data
    web_socket = "http://132.234.39.78:4005";

    # number of rows sent per post request to the web socket
    request_limit = 120;

    # perform error checking
    if(not isinstance(request_limit, int) or request_limit < 1):
        raise Exception("main: request limit must be an integer greater than 1")

    if(not os.path.isdir("./" + fileDirectory)):
        raise Exception("main: the file directory provided by fileDirectory does not exist");

    # continuously process csv files from the spool directory
    while (True):

        # process files with .csv extension
        for sensor_file in os.listdir(fileDirectory + "/"):
            if sensor_file.endswith(".csv"):

                # retrieve data from csv file
                data = process_csv_file(fileDirectory, sensor_file);

                json_data = [];

                while(len(data) > 0):

                    json_data.append(data.pop());

                    # communicate input data to web socket each time we reach the number of rows equal to the request limit (number of rows sent per request)
                    if (len(json_data) >= request_limit or len(data) <= 0):
                    
                        response = process_csv_data(json_data, web_socket);
                        
                        print(response.status_code);
                        
                        if(response.status_code != 200):
                            print("error posting data to web socket");
                        
                        json_data = [];

                try:
                    # delete the file after it has been read and communicated
                    os.remove(fileDirectory + "/" + sensor_file);

                except Exception as e:
                    print("Exception Raised deleting file: {}, Exception: {}".format(sensor_file, e));
                    sys.exit(1);

# configure log file for writing uncaught errors and exceptions
logger = logging.getLogger(__name__);
logging.basicConfig(filename="error.log", filemode='a', level=logging.INFO, format='%(asctime)s %(levelname)-8s %(message)s', datefmt="%d/%m/%Y %I:%M:%S %p");

# handle_uncaught_exception is a function to write the uncaught exceptions to the log file
def handle_uncaught_exception(exc_type, exc_value, exc_traceback):

    if(issubclass(exc_type, KeyboardInterrupt)):
        sys.__excepthook__(exc_type, exc_value, exc_traceback);
        return
    logger.critical("Uncaught Exception", exc_info=(exc_type, exc_value, exc_traceback));

# configure default exception handling to use the method we defined
sys.excepthook = handle_uncaught_exception;

# run main function
if __name__ == '__main__':
    main();
                



