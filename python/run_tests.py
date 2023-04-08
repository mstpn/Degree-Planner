"""
This is a script to run all of the tests in the Python Test Plan document

To run the tests, make sure that you are in the parent directory of /python (i.e. the root project folder)
and run the following command:
    python3 run_tests.py
"""

from graph import build_graph
import student as std
import schedule as shed
import os

if __name__ == '__main__':
    # Build graphs (only need to do this once)
    print('building graph')
    build_graph()

    test_path = "data/input/test/python/"
    num_tests = len(os.listdir(test_path))
    # num = 23
    for i in range(1, num_tests+1):
    # for i in range(1, num+1):
        print("Test " + str(i) + ": ")
        student_input_file = test_path + str(i) + ".json"
        student = std.Student(filename=student_input_file)
        degree, success, msg = shed.degree_handler(student)
        print(msg)
        if success:
            print("num_semesters: " + str(len(degree)))
            print("num_years: " + str(len(degree)/2))
        print("")

