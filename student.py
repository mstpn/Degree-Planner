import pickle
import random
from class_definitions import Course_Node
CORE = ["PHIL1179","MATH1200","MATH1203","MATH1271","MATH2334","COMP1631","COMP1633","COMP2613",
        "COMP2631","COMP2633","COMP2655","COMP2659","COMP3309","COMP3614","COMP3649","COMP3659"]

class Student:
    def __init__(self,**kwargs):
        self.name = kwargs.get("name","N\A")
        self.semester = kwargs.get("semester","N\A")
        self.courses_taken = kwargs.get("courses_taken",[])
        self.chosen_sen_options = kwargs.get("chosen_sen_options",[])
        self.chosen_jun_options = kwargs.get("chosen_jun_options",[])
        self.cognate_choice = kwargs.get("cognate_choice",[])
        self.chosen_grad_year = kwargs.get("chosen_grad_year",2025)

    def __str__(self) -> str:
        str_fmt = "NAME:{}\nSEMESTER: {}\nCOURSE_TAKEN{}\nSEN_OPS{}\nJUN_OPS{}\nCOG{}\nPREFERED_GRAD_YEAR: {}".format(self.name,self.semester,self.courses_taken,self.chosen_sen_options,self.chosen_jun_options,self.cognate_choice,self.chosen_grad_year)
        return str_fmt

    def __repr__(self) -> str:
        return self.__str__()

    def from_args(cls,name,semester,courses_taken,chosen_sen_options,chosen_jun_options,cognate_choice,chosen_grad_year):
        name = name
        semester = semester
        courses_taken = courses_taken
        chosen_sen_options =chosen_sen_options
        chosen_jun_options = chosen_jun_options
        cognate_choice = cognate_choice 
        chosen_grad_year = chosen_grad_year

    
    def make_graph(self):
        reqs = self.all_required_courses()
        pickle_path = 'pickles/all_courses_dict.pkl'
        courses = pickle.load(open(pickle_path, 'rb'))
        node_list = []
        node_dict = {}
        for prereq in reqs:
            node = Course_Node(prereq)
            node_list.append(node)
            node_dict[prereq] = node
        for node in node_list:
            temp_course = courses.get(node.name,None)
            if temp_course is None:
                print('WHY')
                continue
            pre_reqs_for_course = temp_course.prereqs
            for possible_prereqs_and in pre_reqs_for_course:
                for possible_prereq in possible_prereqs_and:
                    prereq = node_dict.get(possible_prereq.name,None)
                    if prereq:
                        node.pre.append(prereq)
                        prereq.next.append(node)

            # next_req_for_course = temp_course.get_nextreqs()
            # node.pre.append(pre_reqs_for_course)
            # node.next.append(next_req_for_course)       

        return node_list, node_dict



    def all_required_courses(self):

        all_required_courses = []
        
        all_required_courses.extend( self.get_sen_ops() )
        all_required_courses.extend( self.get_jun_ops() )
        all_required_courses.extend( self.get_cognate_choice() )
        all_required_courses.extend( CORE )

        print(all_required_courses)

        #remove taken courses``
        all_required_courses = list(set(all_required_courses).difference(student.get_courses()))


        print(student.get_courses())

        print(all_required_courses)

        return all_required_courses





    @classmethod
    def from_file(cls, filename):
        pass
        #return cls(radius=diameter / 2)
    @property
    def completed_courses(self):
        return self.get_courses()

    def add_course(self,course):
        if course not in self.course_taken:
            self.courses_taken.append(course)

    def add_course_list(self,courses):
        if courses not in self.course_taken:
            self.courses_taken.extend(courses)

    def get_courses(self):
        return self.courses_taken

    def has_course(self,course):
        index = self.courses_taken.index(course)
        if index:
            return self.courses_taken[index]
        else:
            return None

    def add_sen_op(self,op):
        if op not in self.chosen_sen_options:
            self.chosen_sen_options.append(op)

    def add_sen_op_list(self,op):
        if op not in self.chosen_sen_options:
            self.chosen_sen_options.extend(op)

    def get_sen_ops(self):
        return self.chosen_sen_options

    def has_sen_op(self,op):
        index = self.chosen_sen_options.index(op)
        if index:
            return self.chosen_sen_options[index]
        else:
            return None

    def get_jun_ops(self):
        return self.chosen_jun_options

    def get_cognate_choice(self):
        return self.chosen_sen_options

    def build_requirements_graph(self):
        pass


def build_student_test():
    return Student(
        name="Soren Edwards",
        semester="Fall",
        courses_taken = ["COMP1633"],
        chosen_sen_options =["COMP4555","COMP5690","COMP4630"],
        chosen_jun_options = ["COMP3533","COMP3625","COMP2521"],
        cognate_choice = ["GEOG1101","GEOG1105","GEOG2553","GEOG3553"],
        chosen_grad_year =  2030)

if __name__ == '__main__':

    student = build_student_test()
    student.all_required_courses()
    print(student.cognate_choice)

    node_list, node_dict = student.make_graph()

    print(done)
