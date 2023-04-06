'''
This file is essentialy a header file for the other files in this project.
'''
import csv
import json
import math

IGNORE_COURSES = [

]

IGNORE_PREREQS = [
    'MATH0135'
]

IGNORE_DEPTS = [
    'XFDC',
    'XMAT',
    'XPFT'
]

# Semester indices
F = 0
W = 1

# A_Class type
LEC = 0
LAB = 1
TUT = 2


# # A students registration
# class Registration():

#     def __init__(self) -> None:
#         self.semesters = []
#         self.num_semesters = 0
#         self.years = 0

#     def get_years(self):
#         return math.ceil(len(self.semesters) / 2)

#     def __str__(self) -> str:
#         for semester in self.semesters:
#             print(semester)


class Semester():
    def __init__(self, year, worf, max_courses) -> None:
        self.year = year
        self.worf = worf
        self.max_courses = max_courses
        self.courses = []  # contains Section objects

    def list_courses(self):
        course_list = []
        for section in self.courses:
            course_list.append(section.course_name)
        return course_list

    def __str__(self) -> str:
        courses = ''
        for section in self.courses:
            courses += section.course_name + '\n'
        if self.worf == F:
            semester = 'Fall'
        else:
            semester = 'Winter'

        return f'Year: {self.year} {semester}:\n{courses}'

    def worf_string(self):
        if self.worf == F:
            return 'F'
        else:
            return 'W'

    def get_year_and_worf(self):
        return f'Year: {self.year} Semester: {self.worf_string()}'


class Course_Node():

    def __init__(self, name) -> None:
        self.name = name
        self.pre = []
        self.next = []
        self.prereqTreeCount = None
        self.taken = False

    def findPrereqCount(self):
        pass

    def to_json(self):
        return {
            'name': self.name,
            'pre': self.pre_to_str_list(),
            'next': self.next_to_str_list(),
        }
    
    def pre_to_str_list(self):
        return [x.name for x in self.pre]
    
    def next_to_str_list(self):
        return [x.name for x in self.next]


class A_Class:
    def __init__(self, id, occurence, start_time, duration, day, prof, room):
        self.id = id  # csv col E
        self.occurence = occurence  # csv col F, time this id occurs
        self.start_time = start_time
        self.duration = duration
        self.day = day
        self.prof = prof
        self.room = room


class Section:
    def __init__(self, course_name, id, description=None):
        self.course_name = course_name
        self.id = id  # Section from CSV (col D)
        self.description = description
        # self.classes = []
        self.class_types = [{}, {}, {}]  # LEC, LAB, TUT
        # TODO rework anything with .classes to .class_types

    def get_all_classes(self):
        all_classes = []
        for class_type in self.class_types:
            for classes in class_type.values():
                for class_ in classes:
                    all_classes.append(class_)
        return all_classes


class Course:
    def __init__(self, name, dept=None, course=None, description=None, semester=None):
        self.name = name
        self.dept = dept
        self.description = description
        self.semester = semester
        self.prereqs = []  # nested lists of classes, separated by AND
        self.sections = {}  # dict so that we can add from the csv easily

    def __str__(self):
        name = self.name
        pre = ''
        if len(self.prereqs) > 0:
            and_list = []
            for prereq_list in self.prereqs:
                or_list = []
                for prereq in prereq_list:
                    or_list.append(prereq.name)
                and_list.append(' or '.join(or_list))
            pre = '\n  Prereqs: ' + ' AND '.join(and_list)
        return name + pre

    # Messy and bad runtime, but it works. Comment out try/except to speed up at cost of accuracy (same # prereqs but not SAME prereqs will pass)
    def __eq__(self, other):
        if self.name != other.name:
            # print('name not equal')
            return False
        if len(self.prereqs) != len(other.prereqs):
            # print('prereqs not equal')
            return False
        try:
            for i in range(len(self.prereqs)):
                if len(self.prereqs[i]) != len(other.prereqs[i]):
                    self_pre = self.prereqs[i]
                    other_pre = other.prereqs[i]
                    # print('prereq ' + str(i) + ' not equal')
                    return False
        except:  # to handle index out of range
            return False
        if len(self.sections) != len(other.sections):
            return False
        return True

        # https://stackoverflow.com/questions/20242479/printing-a-tree-data-structure-in-python
    def __repr__(self, level=0):
        ret = "\t"*level+repr(self.name)+"\n"
        for prereq_list in self.prereqs:
            for prereq in prereq_list:
                ret += prereq.__repr__(level+1)
        return ret
