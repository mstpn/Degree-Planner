import random
class Student:

    def __init__(self,**kwargs):
        self.name = kwargs.get("name","N\A")
        self.semester = kwargs.get("semester","N\A")
        self.courses_taken = kwargs.get("courses_taken",[])
        self.chosen_sen_options = kwargs.get("chosen_sen_options",[])
        self.chosen_jun_options = kwargs.get("chosen_jun_options",[])
        self.cognate_choice = kwargs.get("cognate_choice",random.choice(["GEOG","MATH","PHYS","GEOL","BIOL","CHEM"])) #0:math, 1:GIS
        self.chosen_grad_year = kwargs.get("chosen_grad_year",3000)

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
        cognate_choice = cognate_choice #0:math, 1:GIS
        chosen_grad_year = chosen_grad_year
        

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


    def build_requirements_graph(self):
        pass


def build_student_test():
    return Student(name="Soren Edwards",semester="Fall",courses_taken = ["COMP1631","COMP1633"],chosen_sen_options =["COMP4555","COMP5690","COMP4630"],
        chosen_jun_options = ["COMP3533","COMP3625","COMP2521"],
        cognate_choice = ["GEOG"],
        chosen_grad_year =  2030)



if __name__ == '__main__':
    student = build_student_test()
    print(student.completed_courses)

