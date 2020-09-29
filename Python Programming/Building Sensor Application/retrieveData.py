import nidaqmx
import csv
import os
import sys
import logging
import os.path
import time
from os import path
from nidaqmx.constants import AcquisitionType, Edge 
from datetime import datetime

# setup_task is a function to configure a nidaqmx task to continuously read samples from the cDAQ (Data Acquitision Device)
# Input Parameters:
#   device_name - a string containing the device name for the cDAQ connected to the computer
#   channels    - a list containing the channels that you would like to create virtual channels for (must be contained within the list ["ai0", "ai1", "ai2", "ai3"]
#   clock_rate  - the sample clock rate for the task. Must be a number greater than or requal to 1
#
# Return Parameters:
#   task        - nidaqmx Task that has been configured to communicate with the cDAQ
def setup_task(device_name, channels, clock_rate):
    
    # perform error checking on input parameters
    for i in channels:
        if(i not in ["ai0", "ai1", "ai2", "ai3", "ai4"]):
            raise Exception("setup_task: invalid channel name detected");

    if(not isinstance(device_name, str)):
       raise Exception("setup_task: device name must be a String");

    if(not isinstance(clock_rate, (int, float)) or clock_rate <= 1):
      raise Exception("setup_task: clock_rate must be a number greater than or equal to 1");

    # start a task to access the National Instruments cDAQ 
    task = nidaqmx.Task();

    try:
        # create a virtual channel for each of the channels provided
        for ch in channels:
            task.ai_channels.add_ai_voltage_chan(device_name + "/" + ch);

        # configure the clock rate for the task
        task.timing.cfg_samp_clk_timing(clock_rate, source="", active_edge=Edge.FALLING, sample_mode=AcquisitionType.FINITE, samps_per_chan=75);

    except Exception as e:
        task.close();
        raise Exception("Error configuring task: " + str(e));

    return task

# create_csv_file creates a new csv file with a unique name that can be written to
# Return Parameters:
#   fp    - file pointer to the new csv file that has been created
def create_csv_file():

    # retrieve the current time
    time = datetime.now();

    # create a new csv file to store the input data
    fp = open('N79_Sensor_Data_' + time.strftime("%d_%m_%Y_%H_%M_%S_%f") + ".csv", mode="w", newline='');

    return fp;

# write_data_to_csv is a function to write input data collected from a cDAQ (Data Acquisition Device) until the number of maximum rows for the file have been reached
# Input Parameters:
#   fp          - file pointer containing the csv file to be written to
#   data        - data to be written to csv file (a list, containing lists of [sensor_id, sensor_type, voltage, timestamp])

def write_data_to_csv(fp, data):

    seen = [];
    
    if(len(data) <= 0):
        return;

    # headers for the csv file
    headers = ["sensor_id", "sensor_type", "sensor_value", "sensor_derived_value", "sensor_timestamp"];

    # perform error checking on input parameters
    if(not os.access(fp.name, os.F_OK)):
        raise Exception("write_data_to_csv: file pointed to by fp does not exist");

    if(not os.access(fp.name, os.W_OK) or not os.access(fp.name, os.R_OK)):
        raise Exception("write_data_to_csv: file pointed by fp must have read and write permissions");

    if(not fp.name.endswith(".csv")):
        raise Exception("write_data_to_csv: fp must point to a csv file");

    csv_writer = csv.writer(fp, delimiter=',');

    # write data headers to csv file
    csv_writer.writerow(headers);

    # write data to csv file
    for row in data:
        
        # calculate acceleration measurement for current voltage value
        # As per specifications for acclerometer being used: acceleration = input (mV) / 10
        if(row["type"] == "Accelerometer"):
            derived_value = row["value"] / 10;
        elif(row["type"] == "Strain Gauge"):
            derived_value = row["value"];
        else:
            derived_value = row["value"];
           
        csv_writer.writerow([row["id"], row["type"], row["value"], derived_value, row["timestamp"]]);

def main():

    # device name for the cDAQ
    device_name = "cDAQ3Mod1";

    # define sensor types connected to cDAQ. They should be arranged in the order they are connectected (i.e channel ai0 -> index 0)3
    # NOTE - Each sensor Type should be "Accelerometer" or "Strain Gauge"
    #        Each index of the sensor_type list should map the channel of the associated sensor connected to the cDAQ
    sensor_type = ["Accelerometer", "Accelerometer", "Accelerometer"];

    # channel names for the cDAQ that you want to read inputs from
    # NOTE: the number of channels must be greater than 0 and max_file_rows must be evenly divisible by the number of channels
    #       Channel names should be of the format "ai<channel number>", as recognized by the cDAQ
    channels = ["ai0", "ai1", "ai2"];

    # define sensor ids for the sensors currently connected to the cDAQ
    # NOTE: The number of ids must be equal to the number of channels, with each id being unique
    #       Each index of the sensor id list should map the channel of the associated sensor connected to the cDAQ
    #       Each sensor is should use the following format:
    #               Accelerometers: "Level <floor> Accelerometer <unique identifier>". The unique idenifier can be used when
    #                               multiple accelerometers are located on the same floor.
    #               Strain Gauge: "Strain Gauge <unique identifier>". The unique identifier is currently a number between 1-15. 
    #                              However it can be used in future to provide further context, such as location.
    sensor_ids = ["Level 1 Accelerometer", "Level 3 Accelerometer", "Level 5 Accelerometer"];

    # define the maximum sample clock rate for onboard clock of the cDAQ
    clock_rate = 2000.0;

    # define the subdirectory where the input data files will be stored
    # NOTE: this should be the same spool directory where the input files are being processed
    data_directory = "Sensor Data";

    # define the number of rows that will be written to each csv file (1 row = 1 input from a single sensor connected to the cDAQ)
    # NOTE: max_file_rows should be greater than 0 and must be evenly divisible by the number of channels
    max_file_rows = 1500;

    # configure the task for the cDAQ
    task = setup_task(device_name, channels, clock_rate);

    while(True):
        
        # counter to track the amount of rows to write to csv file
        row_counter = 0;
        
        if(len(channels) < 1 or ((max_file_rows % len(channels)) != 0)):
            raise ValueError('max_file_rows must be evenly divisible by the number of channels');

        if(len(sensor_ids) != len(sensor_type)):
            raise ValueError('the length of sensor_ids must match the length of sensor types');

        for t in sensor_type:
            if(not isinstance(t, str)):
                raise ValueError('each sensor type must be a string');

        
        # list to collect rows of input data from the sensors
        data = [];
        
        while(row_counter < max_file_rows):

            # retrieve the current time
            time = datetime.now();
            
            timestamp = time.strftime("%Y-%m-%dT%H:%M:%S.%f");

            # read data from the input channels
            inputs = task.read();
            
            # perform error checking
            if(len(sensor_ids) != len(inputs)):
                raise ValueError('The number of sensor_ids did not match the number of inputs being read from the cDAQ');

            if(len(inputs) != len(channels)):
                raise ValueError('the number of channels did not match the number of inputs being read from the cDAQ');
       
       
            for i in range(0, len(inputs)):
                row = {
                    "id": sensor_ids[i], 
                    "type": sensor_type[i],
                    "timestamp": timestamp,
                    "value": inputs[i]
                }
            
                data.append(row);
                
                row_counter += 1;

        # create new csv file to write input data to
        f = create_csv_file();

        # write the input data collected from the cDAQ until the csv file has reached the maximum number of rows
        write_data_to_csv(f, data);

        # close the file
        f.close();

        # move the file into the directory defined by data_directory
        cwd = os.getcwd();
        os.replace(os.path.realpath(f.name), cwd + '/' + data_directory + '/' + os.path.basename(f.name));

    # close the nidaqmx task to free up resources
    task.close();

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







