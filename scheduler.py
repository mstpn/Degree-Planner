import student as std
import class_definitions as cd

# SEMESTER INDEXES
F = 0
W = 1

def create_schedule( stud, courses, course_dict ):

    all_courses = stud.all_required_courses()
    graph_nodes_list, graph_nodes_dict = stud.make_graph()

    program = cd.Regi()
    num_semesters = 0
    year = 1

    while len(all_courses) > 0 and year <= stud.years_to_grad:

        semesterObject = cd.Semester(year, stud.semester, stud.maxCoursesPerSemester )

        schedule_semester( all_courses, course_dict, graph_nodes_dict, semesterObject, program, 0 )
        num_semesters += 1
        
        if num_semesters % 2 == 0:
            year += 1

    return program

def schedule_semester( all_courses, course_dict, graph_nodes_dict, semester, program, num_scheduled ):

    next_courses = all_courses.copy()
    maxCoursesPerSemester = semester.maxCourses

    super_safe_double_check = True

    while num_scheduled < maxCoursesPerSemester and super_safe_double_check:
        course_name = all_courses.pop(0)
        course = graph_nodes_dict.get(course_name, None)
        
        if course:
            for prereq in course.pre:
                if not prereq.taken:
                    all_courses.append(course_name)
                    schedule_semester( all_courses, course_dict, graph_nodes_dict, semester, program, num_scheduled )

    



    return 
