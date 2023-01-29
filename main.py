import pickle
from class_definitions import Course, Section, A_Class
import student as std
import scheduler as shed

# SEMESTER INDEXES
F = 0
W = 1

if __name__ == '__main__':
    # Load courses from pickle
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

    program = shed.create_schedule(student, courses, course_dict)

    print('done.fun')

    # test_course_name = 'ENTR4433'
    # test_course = course_dict[F].get(test_course_name, None)
    # if test_course is not None:
    #     print('print course')
    #     print(repr(test_course))