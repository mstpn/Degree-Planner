import pickle
from class_definitions import F, W
from graph import build_graph
import student as std
import scheduler as shed

if __name__ == '__main__':
    # Build graphs (only need to do this once)
    build_graph()

    print('loading data structures')
    courses = [[], []]
    course_dict = [{}, {}]
    pickle_folder = 'pickles/'
    fall_pickle_path = pickle_folder + 'fall_courses.pkl'
    winter_pickle_path = pickle_folder + 'winter_courses.pkl'
    courses[F] = pickle.load(open(fall_pickle_path, 'rb'))
    courses[W] = pickle.load(open(winter_pickle_path, 'rb'))
    for course in courses[F]:
        course_dict[F][course.name] = course
    for course in courses[W]:
        course_dict[W][course.name] = course

    student = std.build_student_test()

    # Single
    print('building graph')
    program = shed.create_schedule(student, course_dict)

    # shortest time 1k loops
    best_program = [10000,None]
    for i in range(1000):
        print(i+1, 'of 1000')
        program = shed.create_schedule(student, course_dict)
        if program.num_semesters < best_program[0]:
            best_program = [program.num_semesters, program]
    program = best_program[1]

    print('writing to csv')
    student.program_to_csv(program)

    print('done.fun')
