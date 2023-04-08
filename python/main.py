"""
This is the main file for the program. It is responsible for building the data structures and running the
program code. It is also responsible for writing the output to a csv file.

To run the program, make sure that you are in the parent directory of /python (i.e. the root project folder)
and run the following command:
    python3 main.py
"""

from graph import build_graph
import student as std
import schedule as shed

if __name__ == '__main__':
    # Build graphs (only need to do this once)
    print('building graph')
    build_graph()

    # Load student data
    student_input_file = "data/input/soren.json"
    student = std.Student(filename=student_input_file)

    # Schedule degree
    degree, success, msg = shed.degree_handler(student)
    print(msg)
    if success:
        print('writing to csv')
        student.program_to_csv(degree)
    print('done')
