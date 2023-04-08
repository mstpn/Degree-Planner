""" 
This file contains the code for scheduling a degree.
The degree is a list of semesters, each of which is a list of courses.
The degree and semesters are scheduled by performing a recursive backtracking search.

Search:
    Recursive backtracking search.
    This will be performed at both the semester and degree level
        If we get to a situation where we can't complete the degree in the alloted time, we backtrack to the previous semeseter and try to schedule a different course.
            So we will need to keep track of the courses that have been scheduled or attempted to be scheduled in each semester.
    The base case is a case in which the solution configuration has been achieved
        In our case this means that either the maximum number of courses per term has been met or no more courses can be taken
    We start with the first course in the list of all required courses, and try to schedule it.
        If we can't schedule it, we move on to the next course in the list.
        If we can schedule it, we move on to the next course in the list.
"""

import class_definitions as cd
import student as std

CANT_SCHED = -1
NO_SECTIONS = -2


def degree_handler(student):
    """
    Handles the scheduling of a degree.
    A degree is a list of semesters, each of which is a list of courses.
    Initiates the recursive backtracking search for semesters
        Each semester contains a backtracking search for courses
    On success, returns the degree, a boolean indicating success, and a message
    On failure, returns an empty degree, a boolean indicating failure, and a message
    """
    # Check for invalid inputs
    if student.years_to_grad <= 0 or student.max_courses_per_semester <= 0:
        success = False
        msg = "\nStudent has an invalid number of semesters to graduate or courses per semester"
        return [], success, msg
    if len(student.all_required) > student.max_courses_per_semester * student.sem_to_grad:
        success = False
        msg = "\nStudent has too many courses to take in the time allotted"
        return [], success, msg
    # Check for completed degree
    if len(student.all_required) == 0:
        success = True
        msg = "\nStudent has already completed their degree"
        return [], success, msg
    # Schedule degree
    courses_remaining = student.sort_all()
    degree = []
    curr_sem = 0
    success = schedule_recursive(
        student, curr_sem, degree, courses_remaining)
    if success:
        msg = f"\nDegree successfully scheduled in %s years (%s semesters)" % (
            len(degree)/2, len(degree))
    else:
        msg = f"\nDegree was not able to be scheduled in %s years" % student.years_to_grad
    return degree, success, msg


def schedule_recursive(student: std.Student, curr_sem: int, degree: list[cd.Semester], courses_remaining: list[str]):
    """
    Recursive function to schedule a degree.
    The recursive case iterates through every possible course to schedule in the remaining courses list.
    If the course can be scheduled, it is added to the current semester and the function is called again.
    If the course cannot be scheduled, the function is called again with the next course in the list.
        This is the backtracking part of the algorithm. 
        Degree is returned to the state it was in before the semester was attempted to be scheduled.
        This ensures all possible permuations of the degree are attempted before the function returns False.
    If the course cannot be scheduled and there are no more courses in the list, the function returns False.
    """
    # base fail case, no degree found after all semesters scheduled
    if len(degree) >= student.sem_to_grad:
        return False
    # base success case, degree valid
    elif len(courses_remaining) == 0:
        return True
    # recursive case, schedule current semester
    else:
        success = False
        i = 0
        while i < len(courses_remaining) and not success:
            degree.append(cd.Semester((curr_sem // 2)+1,
                          student.semester, student.max_courses_per_semester))
            courses_remaining = reorder_courses(courses_remaining, i)
            to_schedule = courses_remaining.copy()
            course_recursive(student, courses_remaining,
                             to_schedule, degree[-1], degree)
            courses_remaining = [
                x for x in courses_remaining if x not in degree[-1].list_courses()]
            curr_sem += 1
            student.change_semester()
            success = schedule_recursive(
                student, curr_sem, degree, courses_remaining)
            if not success:
                # print("Backtracking semester")
                degree = degree[:curr_sem]
                i += 1
    return success


def course_recursive(student, courses_remaining, to_schedule, semester, degree):
    """
    Recursive function to schedule a semester.
    The recursive case iterates through every possible course to schedule in the remaining courses list.
    If the course can be scheduled, it is added to the current semester and the function is called again.
    If the course cannot be scheduled, the function is called again with the next course in the list.
        This is the backtracking part of the algorithm. 
        This ensures all possible permuations of courses are attempted before the function returns False.
    If the course cannot be scheduled and there are no more courses in the list, the function returns False.
    """
    # base case
    if len(semester.courses) >= student.max_courses_per_semester or len(to_schedule) <= 0:
        return True
    # recursive case
    success = False
    while len(to_schedule) > 0 and not success:
        course = to_schedule.pop(0)
        course_details = student.course_dict[semester.worf].get(course, None)
        if course_details and \
                check_prereqs(student, course, degree) and \
                schedule_course(course_details, semester):
            success = course_recursive(
                student, courses_remaining, to_schedule, semester, degree)
            # print("Backtracking course")
    return success


def check_prereqs(student, course, degree):
    """
    Returns True if the course has its prereqs met
    Returns False if the course does not have its prereqs met
    """
    if course == "MATH2101" or course == "MATH4111":
        print("")
    course_details = student.course_dict[degree[-1].worf].get(course, None)
    if course_details is None:
        print("Course not found in graph")
        return False
    pre_list = course_details.list_prereqs()
    courses_taken = student.compute_courses_taken(degree)
    for option_list in pre_list:
        if len(option_list) == 0:
            continue
        taken_option = False
        for option in option_list:
            if option in courses_taken:
                taken_option = True
                break
        if not taken_option:
            return False
    return True


def schedule_course(course, semester):
    """
    Returns True if the course was scheduled
    False if the course could not be scheduled or was already taken
    """
    section = can_schedule(course, semester)
    if section:
        semester.courses.append(section)
        course.taken = True
        return True
    return False


def can_schedule(course, semester):
    """
    Checks if a course can be scheduled in the semester
    The all sections of the course are checked against the courses already scheduled in the semester
        The types[] array ensures that all lectures, labs, and tutorials for a section are valid
    Returns a section of a course that can be scheduled in the semester
    Returns None if no section can be scheduled
    """
    new_section = None

    for section in course.sections.values():
        time_available = True
        types = [CANT_SCHED, CANT_SCHED, CANT_SCHED]
        solve_section(section, semester, types)
        if CANT_SCHED in types:
            time_available = False
            break
        if time_available:
            new_section = cd.Section(
                section.course_name, section.id, section.description
            )
            for index, value in enumerate(types):
                if value != NO_SECTIONS:
                    new_section.class_types[index][
                        value
                    ] = section.class_types[index][value]
            return new_section
    return None


def solve_section(section, semester, types):
    """
    Solves the section by finding if all class types can be scheduled
    """
    for i in range(len(section.class_types)):
        if len(section.class_types[i]) == 0:
            types[i] = NO_SECTIONS
            continue
        solve_class_type(section, semester, types, i)
    return


def solve_class_type(section, semester, types, type_index):
    """
    Checks all class types to see if one can be found that can be scheduled
    """
    for type_section in section.class_types[type_index].values():
        # section_available = True
        if solve_session(type_section, semester):
            types[type_index] = type_section[0].id
            break
    return


def solve_session(type_section, semester):
    """
    Solves the class type by finding if all sessions can be scheduled
    Returns True if all sessions can be scheduled
    Returns False if any session cannot be scheduled
    """
    for session in type_section:
        if time_conflict(session, semester):
            return False
    return True


def time_conflict(a_class, semester):
    """
    Returns True if the class to be scheduled conflicts with any class in the semester
    """
    for section in semester.courses:
        all_classes = section.get_all_classes()
        for section_class in all_classes:
            if a_class.day == section_class.day:
                if overlap(
                    a_class.start_time,
                    a_class.duration,
                    section_class.start_time,
                    section_class.duration,
                ):
                    return True
    return False


def overlap(start1, duration1, start2, duration2):
    """
    Returns True if the two time periods overlap
    """
    end1 = start1 + duration1
    end2 = start2 + duration2
    if start1 <= start2 and end1 >= start2:
        return True
    if start1 <= end2 and end1 >= end2:
        return True
    if start2 <= start1 and end2 >= start1:
        return True
    if start2 <= end1 and end2 >= end1:
        return True
    return False


def reorder_courses(courses, index):
    """
    Returns a list of courses with the courses at index and after moved to the front
    This gives us new orderings of the courses to try on backtracking
    """
    l1 = courses[index:]
    l2 = courses[:index]
    return l1 + l2


if __name__ == "__main__":
    """
    The optional main for testing purposes
    """
    student_input_file = "data/input/soren.json"
    student = std.Student(filename=student_input_file)

    degree, success, msg = degree_handler(student)
    print(msg)
    if success:
        print('writing to csv')
        student.program_to_csv(degree)
