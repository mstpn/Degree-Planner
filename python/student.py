"""
This is the file for actions related to the student object. It contains the Student class and all of its
methods. It handles the loading of student data from a json file, and the creation of the graph nodes,
as well as methods and data relating to a student's degree.
"""

import datetime
import pickle
import csv
import json
from class_definitions import Course_Node, F, W, Semester
CORE = ["PHIL1179", "MATH1200", "MATH1203", "MATH1271", "MATH2234", "COMP1631", "COMP1633", "COMP2613",
        "COMP2631", "COMP2633", "COMP2655", "COMP2659", "COMP3309", "COMP3614", "COMP3649", "COMP3659"]


class Student:
    """
    This class represents a student. It contains all of the data that is relevant to a student's degree,
    including the courses they have taken, the courses they have chosen for their degree, and the
    cognate they have chosen.
        These are read in from the json file that is passed in as a parameter to the constructor.
    The student stores a copy of all the courses that it can take (course_dict) as well as the 
    graph structure and dictionary for its specific degree (graph_dict, graph_list).
    """

    def __init__(self, filename=None, **kwargs):
        if filename:
            with open(filename, 'r') as f:
                data = json.load(f)
            if data:
                # print(data)
                kwargs = data

        self.name = kwargs.get("name", "N\A")
        self.semester = kwargs.get("semester", 0)
        self.courses_taken = kwargs.get("courses_taken", [])
        self.initial_courses_taken = self.courses_taken
        self.chosen_jun_options = kwargs.get("chosen_jun_options", [])
        self.chosen_sen_options = kwargs.get("chosen_sen_options", [])
        self.cognate_name = kwargs.get("cognate_name", "N\A")
        self.cognate_choice = kwargs.get("cognate_choice", [])
        self.years_to_grad = kwargs.get("years_to_grad", 4)
        self.sem_to_grad = self.years_to_grad * 2
        self.max_courses_per_semester = kwargs.get(
            "max_courses_per_semester", 6)
        self.all_required = self.all_required_courses()
        self.graph_nodes_list, self.graph_nodes_dict = self.make_graph()
        self.course_dict = self.make_course_dict()

    def make_course_dict(self):
        """
        This function creates a dictionary of all the courses that the student can take.
        """
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
        return course_dict

    def change_semester(self):
        """
        This function changes the student's semester to the next one.
        """
        self.semester = (self.semester + 1) % 2

    def __str__(self) -> str:
        str_fmt = "NAME:{}\nSEMESTER: {}\nCOURSE_TAKEN{}\nSEN_OPS{}\nJUN_OPS{}\nCOG{}\nPREFERED_GRAD_YEAR: {}".format(
            self.name, self.semester, self.courses_taken, self.chosen_sen_options, self.chosen_jun_options, self.cognate_choice, self.years_to_grad)
        return str_fmt

    def __repr__(self) -> str:
        return self.__str__()

    def make_graph(self):
        """
        This function creates the graph structure of Course_Node's for the student's degree.
        """
        reqs = self.all_required_courses()
        reqs.sort()
        pickle_path = 'pickles/all_courses_dict.pkl'
        courses = pickle.load(open(pickle_path, 'rb'))
        node_list = []
        node_dict = {}
        for prereq in reqs:
            node = Course_Node(prereq)
            node_list.append(node)
            node_dict[prereq] = node

        for node in node_list:
            temp_course = courses.get(node.name, None)
            if temp_course is None:
                print('Invalid Course: ', node.name, ' in make_graph')
                continue
            pre_reqs_for_course = temp_course.prereqs
            for possible_prereqs_and in pre_reqs_for_course:
                for possible_prereq in possible_prereqs_and:
                    prereq = node_dict.get(possible_prereq.name, None)
                    if prereq:
                        node.pre.append(prereq)
                        prereq.next.append(node)

        return node_list, node_dict

    def sort_all(self):
        """
        This function sorts the courses in the student's degree by the number of pre-requisites they have.
        This is used to determine a basline optimal order in which the courses are attempted to be scheduled.
        """
        course_dict = {}
        for is_pre in self.all_required:
            num_pre = 0
            for course in self.all_required:
                if is_pre in self.graph_nodes_dict.get(course).pre_to_str_list():
                    num_pre += 1
            course_dict[is_pre] = num_pre
        sorted_dict = sorted(course_dict.items(),
                             key=lambda x: x[1], reverse=True)
        sorted_list = [key for key, value in sorted_dict]
        return sorted_list

    def compute_courses_taken(self, degree: list[Semester]):
        """
        This function computes the courses that the student has taken based on the degree plan.
        Due to the backtracking involved in scheduling, this function is called after each schedule is attempted
        instead of relying on continually setting the student's courses_taken attribute.
        """
        courses = self.initial_courses_taken.copy()
        for semester in degree:
            courses.extend(semester.list_courses())
        return courses

    def all_required_courses(self):
        """
        This function returns a list of all the courses that the student must take to complete their degree.
        """
        all_req_courses = []
        all_req_courses.extend(CORE)
        all_req_courses.extend(self.cognate_choice)
        all_req_courses.extend(self.chosen_jun_options)
        all_req_courses.extend(self.chosen_sen_options)

        # remove taken courses
        all_req_courses = list(
            set(all_req_courses).difference(self.initial_courses_taken))

        return all_req_courses

    def program_to_csv(self, program):
        """
        This function writes the student's degree plan to a csv file.
        The csv file is saved in the data/out folder.
        """
        csv_folder = 'data/out/'
        # file_desc = self.name + '-' + self.cognate_name + '-' + str(program.get_years()) + '_years'
        file_desc = self.name + '-' + self.cognate_name + \
            '-' + str(len(program)) + '_semesters'
        curr_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        file_name = curr_time + '-' + file_desc + '.csv'
        schedule_file = csv_folder + file_name
        print('Writing to file...')
        csv_header = ['Course', 'Section', 'Description',
                      'DayOfWeek', 'ClassHour', 'ClassDuration', 'Room', 'Prof']
        with open(schedule_file, 'w') as f:
            writer = csv.writer(f)
            writer.writerow([file_desc])
            writer.writerow([''])
            for semester in program:
                # for semester in program.semesters: # old way using Registration
                semester_str = semester.get_year_and_worf()
                writer.writerow(['START ' + semester_str])
                writer.writerow(csv_header)
                for section in semester.courses:
                    for a_class in section.get_all_classes():
                        writer.writerow([section.course_name, str(a_class.id.split(
                            '-')[0]), section.description, a_class.day, a_class.start_time, a_class.duration, a_class.room, a_class.prof])
                writer.writerow(['END ' + semester_str])
                writer.writerow([''])


def build_student_test():
    """
    This function builds a test student object.
    """
    return Student(
        name="Soren Edwards",
        semester=F,
        courses_taken=["COMP1631"],
        chosen_sen_options=["COMP4555", "COMP5690", "COMP4630"],
        chosen_jun_options=["COMP3533", "COMP3625", "COMP2521"],
        cognate_name="GEOG",
        cognate_choice=["GEOG1101", "GEOG1105", "GEOG2553", "GEOG3553"],
        years_to_grad=8,
        max_courses_per_semester=6)


if __name__ == '__main__':

    student_input_file = "data/input/soren.json"
    student = Student(filename=student_input_file)

    print(student.cognate_choice)

    node_list, node_dict = student.make_graph()

    print("done")

    # import json
    # with open("sample.json", "w") as outfile:
    #     outfile.write('{')
    #     for node in node_list:
    #         outfile.write('\"' + str(node.name) + '\":')
    #         json.dump(node.to_json(), outfile)
    #         outfile.write(',')
    #     outfile.write('}')
