'''
This file is essentialy a header file for the other files in this project.
'''

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

class Regi():

    def __init__(self, semesters = []) -> None:
        self.semesters = semesters

class Semester():

    def __init__(self,year,worf, maxCourses, courses = []) -> None:
        self.year = year
        self.worf = worf
        self.courses = courses
        self.maxCourses = maxCourses
 
class Course_Node():
    
    def __init__(self,name) -> None:
        self.name = name
        self.pre = []
        self.next = []
        self.prereqTreeCount = None
        self.taken = False
        
    def findPrereqCount(self):
        pass


class A_Class:
    def __init__(self, id, start_time, duration, day, prof, room):
        self.id = id  # Comp-Del from CSV (concat cols E-F)
        if len(id) >= 3:
            if id[0] == '4':
                self.type = 'LAB'
            elif id[0] == '5':
                self.type = 'TUT'
        else:
            self.type = 'LEC'
        self.start_time = start_time
        self.duration = duration
        self.day = day
        self.prof = prof
        self.room = room


class Section:
    def __init__(self, course_name, id):
        self.course_name = course_name
        self.id = id  # Section from CSV (col D)
        self.classes = []


class Course:
    def __init__(self, name, dept=None, course=None, description=None, semester=None):
        self.name = name
        self.dept = dept
        self.description = description
        self.semester = semester
        self.prereqs = [] # nested lists of classes, separated by AND
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
