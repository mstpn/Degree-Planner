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
