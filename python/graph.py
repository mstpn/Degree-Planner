"""
This file contains the code that build our basic trees and dictionaries from the web scraped data.
The build_graph() function is the main function that should be called to build the data structures.
    This is done in the main.py and run_tests.py files.
        Note that it only needs to be done once, and the data structures are saved to files
        in the data folder.
        It is ok to leave build_graph() uncommented as the runtime is very short (1-2 seconds)
        and it doesn't have any negative side effects.
"""

import json
import pickle
import csv
from class_definitions import Course, Section, A_Class, IGNORE_COURSES, IGNORE_PREREQS, IGNORE_DEPTS, F, W, LEC, LAB, TUT
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
                section = Section(
                    course_name, row[SECTION], course.description)
                course.sections[section.id] = section
            id = row[COMP]
            occurence = row[DEL]
            start_time = row[CLASS_HOUR]
            duration = row[CLASS_DURATION]
            day = row[CLASS_DAY_OF_WEEK]
            prof = row[PROF_FIRST] + ' ' + row[PROF_LAST]
            room = row[ROOM]
            a_class = A_Class(id, occurence, start_time,
                              duration, day, prof, room)
            type_section = section.class_types[int(get_class_type(id))]
            type_section_class = type_section.get(id, None)

            if type_section_class is None:
                type_section[id] = [a_class]
            else:
                type_section[id].append(a_class)
            if len(section.class_types) > 3:
                print('big no good')

    # Prune courses with no lecture sections
    for course in courses:
        has_sections = False
        for section in course.sections:
            if len(course.sections[section].class_types[0]) != 0:
                has_sections = True
        if not has_sections:
            courses.remove(course)
    return


def build_graph():
    '''
    This function builds the graphs of courses and stores them in pickle objects
    '''
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
    pickle.dump(all_courses_dict, open(
        pickle_folder + all_courses_dict_name, 'wb'))
    pickle.dump(courses[F], open(pickle_folder + fall_pickle_name, 'wb'))
    pickle.dump(courses[W], open(pickle_folder + winter_pickle_name, 'wb'))


def get_class_type(id):
    if len(id) >= 3:
        if id[0] == '5':
            type_ = LAB
        elif id[0] == '4':
            type_ = TUT
        else:
            type_ = LEC
    else:
        type_ = LEC

    return int(type_)


def courses_json():
    '''
    This function returns a json object of all the courses, also writes to a json file
    '''
    with open('data/courses.csv', 'r') as f:
        reader = csv.reader(f)
        all_course_dict = {}
        course_dict = {}
        prereq_row = 0
        id = ""
        for row in reader:
            if row[0] == "START":
                course_dict = {}
                prereq_row = 0
                row = next(reader)
                id = str(row[0])
                all_course_dict[id] = {}
            elif row[0] == "END":
                all_course_dict[id] = course_dict
            else:
                or_courses = []
                for col in row:
                    or_courses.append(col)
                course_dict[prereq_row] = or_courses
                prereq_row += 1

    json.dump(all_course_dict, open('json/all_courses.json', 'w'))
    return json.dumps(all_course_dict)


if __name__ == '__main__':
    print("Building Graph")
    build_graph()
    course_json = courses_json()
    print("Done")
