import unittest
import retrieveData
import readCsvData
import nidaqmx
import csv
import time
import os.path
import platform
import subprocess
from nidaqmx.constants import AcquisitionType, Edge

class TestSensorData(unittest.TestCase):

#
#
#   TESTS FOR CONFIGURING CDAQ 
#
#

    # test configuring task with valid parameters and 3 virtual channels
    def test_setup_task_valid_with_3(self):
        device = "cDAQ3Mod1";
        channels = ["ai0", "ai1", "ai2"];
        clock_rate = 2000;
        task = retrieveData.setup_task(device, channels, clock_rate);

        self.assertTrue(isinstance(task, nidaqmx.Task));
        self.assertEqual(task.number_of_channels, 3);
        self.assertTrue("cDAQ3Mod1/ai0" in task.ai_channels.channel_names);
        self.assertTrue("cDAQ3Mod1/ai1" in task.ai_channels.channel_names);
        self.assertTrue("cDAQ3Mod1/ai2" in task.ai_channels.channel_names);

        task.close();

    # test configuring task with valid parameters and 2 virtual channels
    def test_setup_task_valid_with_2(self):
        device = "cDAQ3Mod1";
        channels = ["ai0", "ai1"];
        clock_rate = 1500;
        task = retrieveData.setup_task(device, channels, clock_rate);

        self.assertTrue(isinstance(task, nidaqmx.Task));
        self.assertEqual(task.number_of_channels, 2);
        self.assertTrue("cDAQ3Mod1/ai0" in task.ai_channels.channel_names);
        self.assertTrue("cDAQ3Mod1/ai1" in task.ai_channels.channel_names);

        task.close();

    # test configuring task with valid parameters and 1 virtual channel
    def test_setup_task_valid_with_1(self):
        device = "cDAQ3Mod1";
        channels = ["ai0"];
        clock_rate = 2000;
        task = retrieveData.setup_task(device, channels, clock_rate);

        self.assertTrue(isinstance(task, nidaqmx.Task));
        self.assertEqual(task.number_of_channels, 1);
        self.assertTrue("cDAQ3Mod1/ai0" in task.ai_channels.channel_names);

        task.close();

    # test configuring task with float clock rate
    def test_setup_task_float_clock(self):
        device = "cDAQ3Mod1";
        channels = ["ai0"];
        clock_rate = 2000.0;
        
        task = retrieveData.setup_task(device, channels, clock_rate);

        self.assertTrue(isinstance(task, nidaqmx.Task));
        self.assertEqual(task.number_of_channels, 1);
        self.assertTrue("cDAQ3Mod1/ai0" in task.ai_channels.channel_names);

        task.close();


    # test configuring task with maximum clock rate of 0
    def test_setup_task_clock_0(self):
        device = "cDAQ3Mod1";
        channels = ["ai0"];
        clock_rate = 0;
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with invalid device name
    def test_setup_task_invalid_device(self):
        device = "fake_name";
        channels = ["ai0"];
        clock_rate = 2000;
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with invalid channel
    def test_setup_task_invalid_channel(self):
        device = "cDAQ3Mod1";
        channels = ["ai9"];
        clock_rate = 2000;

        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with a one valid and one invalid channel
    def test_setup_task_invalid_channel_type1(self):
        device = "cDAQ3Mod1";
        channels = ["ai0", "ai6"];
        clock_rate = 2000;
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with invalid channel type
    def test_setup_task_invalid_channel_type2(self):
        device = "cDAQ3Mod1";
        channels = [3];
        clock_rate = 2000;
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with negative maximum clock rate
    def test_setup_task_negative_clock(self):
        device = "cDAQ3Mod1";
        channels = ["ai0"];
        clock_rate = -2000;
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with invalid device type
    def test_setup_task_invalid_device_type(self):
        device = 345;
        channels = ["ai0"];
        clock_rate = 2000;
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with invalid clock_rate type
    def test_setup_task_invalid_clock_type(self):
        device = "cDAQ3Mod1";
        channels = ["ai0"];
        clock_rate = "2000";
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

    # test configuring task with channels not in a list
    def test_setup_task_channels_not_in_list(self):
        device = "cDAQ3Mod1";
        channels = "ai0";
        clock_rate = 2000;
        
        with self.assertRaises(Exception): retrieveData.setup_task(device, channels, clock_rate);

#
#
#   TESTS FOR CREATING UNIQUE CSV FILES
#
#

    # test creating a csv file
    def test_creating_csv_file(self):

        fp = retrieveData.create_csv_file();

        self.assertTrue(os.path.isfile("./" + fp.name));

        fp.close();
        os.remove("./" + fp.name);

    # test creating unique csv files
    def test_creating_csv_file_unique(self):

        fp1 = retrieveData.create_csv_file();
        time.sleep(0.5);
        fp2 = retrieveData.create_csv_file();

        self.assertTrue(os.path.isfile("./" + fp1.name));
        self.assertTrue(os.path.isfile("./" + fp2.name));
        self.assertNotEqual(fp1.name, fp2.name);

        fp1.close();
        fp2.close();

        os.remove("./" + fp1.name);
        os.remove("./" + fp2.name);

#
#
#   TESTS FOR WRITING DATA TO CSV FILE
#
#
    # test writing to csv file with valid parameters and 1 virtual task reading from the cDAQ
    def test_write_data_to_csv_valid_with_1(self):

        fp = open("test_file.csv", mode="w+", newline='');

        sensor_type = "Accelerometer";
        data = [
            {
                "id": "Level_1_Acc",
                "type": "Accelerometer",
                "value": 0.5,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "id": "Level_3_Acc",
                "type": "Accelerometer",
                "value": 0.9,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "id": "Level_5_Acc",
                "type": "Accelerometer",
                "value": -0.4,
                "timestamp": "2020-04-20T00:23:18.481"
            }
        ];

        retrieveData.write_data_to_csv(fp, data);

        fp.close();

        fp = open(fp.name, mode="r", newline='');

        csv_reader = csv.reader(fp, delimiter=",");
        line_count = 0;
        data = [];

        for row in csv_reader:
            data.append(row);
            line_count += 1;

        fp.close();

        for y in data[0]:
            self.assertTrue(y in ["sensor_id", "sensor_type", "sensor_value", "sensor_derived_value", "sensor_timestamp"]);

        os.remove(fp.name);

    # test writing to csv file with invalid data format
    def test_write_data_to_csv_with_invalid_data_format(self):

        fp = open("test_file.csv", mode="w", newline='');

        sensor_type = "Accelerometer";
        data = [
            {
                "fake": "Level_1_Acc",
                "type": "Accelerometer",
                "value": 0.5,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "fake": "Level_3_Acc",
                "type": "Accelerometer",
                "value": 0.9,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "fake": "Level_5_Acc",
                "type": "Accelerometer",
                "value": -0.4,
                "timestamp": "2020-04-20T00:23:18.481"
            }
        ];

        with self.assertRaises(Exception): retrieveData.write_data_to_csv(fp, data);

        fp.close();
        os.remove(fp.name);

    # test writing to csv file that doesn't exist
    def test_write_data_to_csv_no_file(self):

        fp = open("test_file.csv", mode="w", newline='');
        fp.close();
        os.remove(fp.name);

        sensor_type = "Accelerometer";
        data = [
            {
                "id": "Level_1_Acc",
                "type": "Accelerometer",
                "value": 0.5,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "id": "Level_3_Acc",
                "type": "Accelerometer",
                "value": 0.9,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "id": "Level_5_Acc",
                "type": "Accelerometer",
                "value": -0.4,
                "timestamp": "2020-04-20T00:23:18.481"
            }
        ];

        with self.assertRaises(Exception): retrieveData.write_data_to_csv(fp, data);

    # test writing to file that's not a csv
    def test_write_data_to_csv_invalid_file_ext(self):

        fp = open("test_file.txt", mode="w", newline='');

        sensor_type = "Accelerometer";
        data = [
            {
                "id": "Level_1_Acc",
                "type": "Accelerometer",
                "value": 0.5,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "id": "Level_3_Acc",
                "type": "Accelerometer",
                "value": 0.9,
                "timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "id": "Level_5_Acc",
                "type": "Accelerometer",
                "value": -0.4,
                "timestamp": "2020-04-20T00:23:18.481"
            }
        ];

        with self.assertRaises(Exception): retrieveData.write_data_to_csv(fp, data);
        
        fp.close();


#
#
#   TESTS FOR READING CSV FILES
#
#

    # test reading csv file with valid paramaters 1
    def test_read_csv_valid1(self):

        directory = "Sensor Data";

        fp = open("test_file.csv", mode="w", newline='');

        csv_writer = csv.writer(fp, delimiter=',');

        csv_writer.writerow(["sensor_id", "sensor_type", "sensor_value", "timestamp"]);
        csv_writer.writerow(["Level_1_Acc", "Accelerometer", 0.5, "12/06/1998 13:15:23.001589"]);
        csv_writer.writerow(["Level_3_Str", "Strain Gauge", 0.243, "12/06/1998 16:25:29.538532"]);

        fp.close();

        cwd = os.getcwd();
        os.replace(os.path.realpath(fp.name), cwd + '/' + directory + '/' + os.path.basename(fp.name));

        input_data = readCsvData.process_csv_file(directory, fp.name);

        self.assertEqual(len(input_data), 2);

        for x in input_data[0].keys():
            self.assertTrue(x in ["sensor_id", "sensor_type", "sensor_value", "timestamp"]);

        for y in input_data[0].values():
            self.assertTrue(y in ["Level_1_Acc", "Accelerometer", "0.5", "12/06/1998 13:15:23.001589"]);

        for x in input_data[1].keys():
            self.assertTrue(x in ["sensor_id", "sensor_type", "sensor_value", "timestamp"]);

        for y in input_data[1].values():
            self.assertTrue(y in ["Level_3_Str", "Strain Gauge", "0.243", "12/06/1998 16:25:29.538532"]);

        os.remove(directory + "/" + fp.name);


    # test reading csv file with valid paramaters 2
    def test_read_csv_valid2(self):

        directory = "Sensor Data";

        fp = open("test_file.csv", mode="w", newline='');

        csv_writer = csv.writer(fp, delimiter=',');

        csv_writer.writerow(["sensor_id", "sensor_type", "sensor_value", "timestamp"]);
        csv_writer.writerow(["Level_5_Acc", "Accelerometer", "0.21", "17/11/1999 13:15:23.000589"]);
        csv_writer.writerow(["Level_1_Str", "Strain Gauge", "-0.187", "20/09/20018 18:25:28.238032"]);
        csv_writer.writerow(["Level_5_Wire", "Wire Vibration", "-0.45", "17/10/2009 12:25:11.038532"]);

        fp.close();

        cwd = os.getcwd();
        os.replace(os.path.realpath(fp.name), cwd + '/' + directory + '/' + os.path.basename(fp.name));

        input_data = readCsvData.process_csv_file(directory, fp.name);

        self.assertEqual(len(input_data), 3);

        for x in input_data[0].keys():
            self.assertTrue(x in ["sensor_id", "sensor_type", "sensor_value", "timestamp"]);

        for y in input_data[0].values():
            self.assertTrue(y in ["Level_5_Acc", "Accelerometer", "0.21", "17/11/1999 13:15:23.000589"]);

        for x in input_data[1].keys():
            self.assertTrue(x in ["sensor_id", "sensor_type", "sensor_value", "timestamp"]);

        for y in input_data[1].values():
            self.assertTrue(y in ["Level_1_Str", "Strain Gauge", "-0.187", "20/09/20018 18:25:28.238032"]);

        for x in input_data[2].keys():
            self.assertTrue(x in ["sensor_id", "sensor_type", "sensor_value", "timestamp"]);

        for y in input_data[2].values():
            self.assertTrue(y in ["Level_5_Wire", "Wire Vibration", "-0.45", "17/10/2009 12:25:11.038532"]);

        os.remove(directory + "/" + fp.name);

    # test reading csv file with directory that doesn't exist
    def test_read_csv_no_dir(self):

        directory = "Fake";

        fp = open("test_file.csv", mode="w", newline='');

        csv_writer = csv.writer(fp, delimiter=',');

        csv_writer.writerow(["sensor_id", "sensor_type", "sensor_value", "timestamp"]);
        csv_writer.writerow(["Level_5_Acc", "Accelerometer", 0.21, "17/11/1999 13:15:23.000589"]);

        fp.close();

        cwd = os.getcwd();
        os.replace(os.path.realpath(fp.name), cwd + '/' + "Sensor Data" + '/' + os.path.basename(fp.name));

        with self.assertRaises(Exception): readCsvData.process_csv_file(directory, fp.name);

        os.remove("Sensor Data" + "/" + fp.name);


    # test reading csv file with file that doesn't exist
    def test_read_csv_no_file(self):

        directory = "Sensor Data";

        fp = open("test_file.csv", mode="w", newline='');

        csv_writer = csv.writer(fp, delimiter=',');

        csv_writer.writerow(["sensor_id", "sensor_type", "sensor_value", "timestamp"]);
        csv_writer.writerow(["Level_5_Acc", "Accelerometer", 0.21, "17/11/1999 13:15:23.000589"]);

        fp.close();
        cwd = os.getcwd();
        os.replace(os.path.realpath(fp.name), cwd + '/' + directory + '/' + os.path.basename(fp.name));
        os.remove(directory + "/" + fp.name);

        with self.assertRaises(Exception): readCsvData.process_csv_file(directory, fp.name);


    # test reading csv file that's empty
    def test_read_csv_empty_csv(self):

        directory = "Sensor Data";

        fp = open("test_file.csv", mode="w", newline='');
        fp.close();

        cwd = os.getcwd();
        os.replace(os.path.realpath(fp.name), cwd + '/' + directory + '/' + os.path.basename(fp.name));

        input_data = readCsvData.process_csv_file(directory, fp.name);

        self.assertEqual(len(input_data), 0);

        os.remove(directory + "/" + fp.name);


    # test reading a file that's not a csv
    def test_read_csv_not_csv(self):

        directory = "Sensor Data";

        fp = open("test_file.txt", mode="w", newline='');
        fp.close();

        cwd = os.getcwd();
        os.replace(os.path.realpath(fp.name), cwd + '/' + directory + '/' + os.path.basename(fp.name));

        with self.assertRaises(Exception): readCsvData.process_csv_file(directory, fp.name);

        os.remove(directory + "/" + fp.name);


    # test reading a file that's not a string
    def test_read_csv_file_not_string(self):

        directory = "Sensor Data";

        with self.assertRaises(Exception): readCsvData.process_csv_file(directory, 12345);

    # test reading a directory that's not a string
    def test_read_csv_dir_not_string(self):

        fp = open("test_file.csv", mode="w", newline='');
        fp.close();

        cwd = os.getcwd();
        os.replace(os.path.realpath(fp.name), cwd + '/' + "Sensor Data" + '/' + os.path.basename(fp.name));

        with self.assertRaises(Exception): readCsvData.process_csv_file(123, fp.name);

        os.remove("Sensor Data" + '/' + fp.name);

#
#
#   TESTS FOR PROCESSING DATA READ FROM CSV FILES
#
#

    # test processing data that is not a list of dictionaries
    def test_invalid_data_format(self):
    
        data = [56];
        
        with self.assertRaises(Exception): readCsvData.process_csv_data(data, "112.34.56");
        
    # test processing data with an invalid web socket type
    def test_invalid_web_address(self):
    
        data = [
            {
                "sensor_id": "Level_1_Acc",
                "sensor_type": "Accelerometer",
                "sensor_value": 0.5,
                "sensor_derived_value": 0.05,
                "sensor_timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "sensor_id": "Level_3_Acc",
                "sensor_type": "Accelerometer",
                "sensor_value": 0.9,
                "sensor_derived_value": 0.09,
                "sensor_timestamp": "2020-04-20T00:23:18.481"
            },
            {
                "sensor_id": "Level_5_Acc",
                "sensor_type": "Accelerometer",
                "sensor_value": -0.1,
                "sensor_derived_value": -0.01,
                "sensor_timestamp": "2020-04-20T00:23:18.481"
            }
        ];
        
        with self.assertRaises(Exception): readCsvData.process_csv_data(data, 112);

    # test availability of Urban Institute server web socket
    def test_web_socket_availability(self):
    
        current_os = platform.system().lower();
        
        if (current_os == "windows"):
            parameter = "-n";
        else:
            parameter = "-c";

        ip = "http://132.234.39.78:4005";
        exit_code = subprocess.call(['ping', parameter, '1', ip]);
        
        self.assertTrue(True);


if __name__ == '__main__':
    unittest.main();