'''
This file is essentialy a header file for the other files in this project.
It contains the class definitions for our data structures as well as some constants
that are used throughout the project.
'''
import math

IGNORE_COURSES = [

]

IGNORE_PREREQS = [
    'MATH0135',
    'MATH0130',
    'MATH1283',
    'MATH1285',
]

IGNORE_DEPTS = [
    'XFDC',
    'XMAT',
    'XPFT',
    'GNED',
]

# Semester indices
F = 0
W = 1

# A_Class type
LEC = 0
LAB = 1
TUT = 2


class Semester():
    """
    A class to represent a semester in a degree program
    """

    def __init__(self, year, worf, max_courses) -> None:
        self.year = year
        self.worf = worf  # 0 for fall, 1 for winter
        self.max_courses = max_courses
        self.courses = []  # contains Section objects

    def list_courses(self):
        """
        Returns a list of the course names in the semester
        """
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
    """
    The course node is used to build the graph of courses that the student
    will actually take.
    It is based off the student json and courses required to graduate.
    """

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
    """
    A specific session of a course
        lab
        tutorial
        lecture
    Contains all the detailed information about that session
    """

    def __init__(self, id, occurence, start_time, duration, day, prof, room):
        self.id = id  # csv col E
        self.occurence = occurence  # csv col F, time this id occurs
        self.start_time = start_time
        self.duration = duration
        self.day = day
        self.prof = prof
        self.room = room


class Section:
    """
    A section of a course.
    Contains all the information about a course that is not specific to a session
    Contains all of the lectures, labs, and tutorials for that course
        Each of these is a list of A_Class objects
    """

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
    """
    A course is a class that is offered by the university
    Contains all the information about a course that is not specific to a section
    """

    def __init__(self, name, dept=None, course=None, description=None, semester=None):
        self.name = name
        self.dept = dept
        self.description = description
        self.semester = semester
        self.prereqs = []  # nested lists of classes, separated by AND
        self.sections = {}  # dict so that we can add from the csv easily

    def list_prereqs(self):
        """
        Returns a list of the names of prereqs in the course
        """
        prereq_list = []
        for prereq_group in self.prereqs:
            group = []
            for prereq in prereq_group:
                if prereq.name not in IGNORE_PREREQS and prereq.dept not in IGNORE_DEPTS:
                    group.append(prereq.name)
            prereq_list.append(group)
        return prereq_list

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
