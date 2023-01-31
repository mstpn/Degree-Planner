import pickle
import csv
from class_definitions import Course, Section, A_Class, IGNORE_COURSES, IGNORE_PREREQS, IGNORE_DEPTS
from student import Student
# CSV COLUMN INDEXES
DEPT = 0
COURSE = 1
DESCRIPTION = 2
SECTION = 3
COMP = 4
DEL = 5
START_MONTH = 6
START_DAY = 7
END_MONTH = 8
END_DAY = 9
CLASS_DAY_OF_WEEK = 10
CLASS_HOUR = 11
CLASS_DURATION = 12
ROOM = 13
PROF_FIRST = 14
PROF_LAST = 15

# SEMESTER INDEXES
F = 0
W = 1


def prereq_from_csv(csv_path):
    '''
    This function reads in a csv file and returns a list of Course objects.
    The csv file should be formatted as follows:
        START
        COURSE NAME
        PREREQ LIST 1 (all related by ORs)
        PREREQ LIST 2 (all related by ORs)
        ...And so on, where each PREREQ LIST is related by ANDs
        END
    Returns a list of Course objects and a dictionary of course names to Course objects
    '''
    # create the a list of course objects with only the names of the prereqs
    courses_name_only = []
    with open(csv_path, 'r') as f:
        reader = csv.reader(f)
        for row in reader:
            if row[0] == 'START':
                course_name = next(reader)[0]
                courses_name_only.append(Course(course_name))
            elif row[0] == 'END':
                continue
            else:
                courses_name_only[-1].prereqs.append(row)

    # create the course objects
    courses = []
    course_dict = {}
    for old_course in courses_name_only:
        if old_course.name in IGNORE_COURSES:
            continue
        course = Course(old_course.name)
        courses.append(course)
        course_dict[course.name] = course

    # add the prereqs to the course objects
    for old_course in courses_name_only:
        course = course_dict[old_course.name]
        for prereq_list in old_course.prereqs:
            if len(prereq_list) < 1:
                continue
            prereq = []
            for prereq_name in prereq_list:
                if prereq_name not in IGNORE_PREREQS:
                    prereq_course = course_dict.get(
                        prereq_name, Course(prereq_name))
                    prereq.append(prereq_course)
            if len(prereq) > 0:
                course.prereqs.append(prereq)

    return courses, course_dict


def fill_course_data(courses, course_dict, semester):
    '''
    This function fills the course data for the courses in the given semester

    Returns nothing, but modifies the courses list and course_dict
    '''

    if semester == 'F':
        csv_name = 'data/fall.csv'
    else:
        csv_name = 'data/winter.csv'

    # add the course data from the csv
    with open(csv_name, 'r') as f:
        reader = csv.reader(f)
        for row in reader:
            course_name = row[COURSE]
            course = course_dict.get(course_name, None)
            if course is None:
                continue
            if course.dept is None or course.description is None:
                course.dept = row[DEPT]
                course.description = row[DESCRIPTION]
                course.semester = semester

            section = course.sections.get(row[SECTION], None)
            if section is None:
                section = Section(course_name, row[SECTION], course.description)
                course.sections[section.id] = section
            id = row[COMP] + '-' + row[DEL]
            start_time = row[CLASS_HOUR]
            duration = row[CLASS_DURATION]
            day = row[CLASS_DAY_OF_WEEK]
            prof = row[PROF_FIRST] + ' ' + row[PROF_LAST]
            room = row[ROOM]
            a_class = A_Class(id, start_time, duration, day, prof, room)
            section.classes.append(a_class)

    # Prune courses with no sections
    for course in courses:
        for section in course.sections:
            if len(course.sections[section].classes) == 0:
                courses.remove(course)
    return


def student_eligible(course, student):
    '''
    This function checks if the student is eligible to take the given course

    Returns True if the student is eligible, False otherwise
    '''
    for rows_of_prereqs_connected_by_ands in course.prereqs:

        eligible = False

        for or_prereq in rows_of_prereqs_connected_by_ands:
            
                if or_prereq.name in student.completed_courses:
                    eligible = True
                    continue

    return eligible


    
def build_graph():
    courses = [[], []]
    course_dict = [{}, {}]
    data_folder = 'data/'
    pickle_folder = 'pickles/'

    # PICKLE METHOD
    course_pickle_name = 'courses_no_semester.pkl'
    pickle_path = pickle_folder + course_pickle_name
    courses_to_dict = pickle.load(open(pickle_path, 'rb'))
    all_courses_dict = {}
    for course in courses_to_dict:
        all_courses_dict[course.name] = course

    courses[F] = pickle.load(open(pickle_path, 'rb'))
    courses[W] = pickle.load(open(pickle_path, 'rb'))
    for course in courses[F]:
        course_dict[F][course.name] = course
    for course in courses[W]:
        course_dict[W][course.name] = course

    # # CSV METHOD
    # csv_name = 'courses.csv'
    # csv_path = data_folder + csv_name
    # courses[F], course_dict[F] = prereq_from_csv(csv_path)
    # courses[W], course_dict[W] = prereq_from_csv(csv_path)

    # fill the course data and remove courses with no sections for the given semester
    fill_course_data(courses[F], course_dict[F], 'F')
    fill_course_data(courses[W], course_dict[W], 'W')

    # pickle the two semester lists & master course list
    fall_pickle_name = 'fall_courses.pkl'
    winter_pickle_name = 'winter_courses.pkl'
    all_courses_dict_name = 'all_courses_dict.pkl'
    pickle.dump(all_courses_dict, open(pickle_folder + all_courses_dict_name, 'wb'))
    pickle.dump(courses[F], open(pickle_folder + fall_pickle_name, 'wb'))
    pickle.dump(courses[W], open(pickle_folder + winter_pickle_name, 'wb'))


if __name__ == '__main__':
    print("Building Graph")
    build_graph()
    print("Done")