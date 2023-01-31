import student as std
import class_definitions as cd

# SEMESTER INDEXES
F = 0
W = 1


def create_schedule(stud, courses, course_dict):

    all_courses = stud.all_required_courses()
    courses_left = all_courses.copy()

    graph_nodes_list, graph_nodes_dict = stud.make_graph()

    program = cd.Regi()
    num_semesters = 0
    year = 1
    semester_worf = stud.semester

    while len(all_courses) > 0 and year <= stud.years_to_grad:

        semester_object = cd.Semester(
            year, semester_worf, stud.max_courses_per_semester)

        semester_full = schedule_semester(
            courses_left, course_dict, graph_nodes_dict, semester_object, program)
        if not semester_full:
            #! right now we have all possible core/required courses scheduled, but still room for electives
            # TODO schedule_gned()
            pass
        num_semesters += 1
        program.semesters.append(semester_object)
        remove_scheduled_courses(all_courses, semester_object)

        # prep for next semester
        courses_left = all_courses.copy()
        print(semester_object)
        if semester_worf == F:
            semester_worf = W
        else:
            semester_worf = F
        if num_semesters % 2 == 0:
            year += 1

    return program


def remove_scheduled_courses(all_courses, semester_object):
    for sections in semester_object.courses:
        try:
            all_courses.remove(sections.course_name)
        except:
            print(sections.course_name), " not in all_courses"
    return


def schedule_semester(all_courses, course_dict, graph_nodes_dict, semester, program):

    if len(all_courses) == 0:
        return False
    next_courses = all_courses.copy()
    max_courses_per_semester = semester.max_courses

    schedule_slots_available = True

    while len(semester.courses) < max_courses_per_semester and schedule_slots_available and len(all_courses) > 0:
        course_name = all_courses.pop(0)
        course_node = graph_nodes_dict.get(course_name, None)
        has_prereqs = True
        if course_node and not course_node.taken:

            for prereq in course_node.pre:
                if not prereq.taken:
                    has_prereqs = False
                    break
                    # super_safe_double_check = schedule_semester(
                    #     all_courses, course_dict, graph_nodes_dict, semester, program)
                    # all_courses.append(course_name)
            if not has_prereqs:
                continue
            course_details = course_dict[semester.worf].get(course_name, None)
            if course_details is None:
                continue
            if schedule_course(course_details, semester):
                course_node.taken = True
                continue
                # for prereq in course_node.pre:
                #     prereq.taken = True
            else:
                schedule_slots_available = schedule_semester(
                    all_courses, course_dict, graph_nodes_dict, semester, program)
        if not course_node.taken:
            all_courses.append(course_name)
    if len(semester.courses) < semester.max_courses or schedule_slots_available is False:
        # still courses slots available to schedule
        return False
    # all courses scheduled
    return True


def schedule_course(course, semester):
    '''
    Returns True if the course was scheduled
    False if the course could not be scheduled or was already taken
    '''
    section = can_schedule(course, semester)
    if section:
        semester.courses.append(section)
        course.taken = True
        return True
    return False


def can_schedule(course, semester):
    '''
    Returns a section of a course that can be scheduled in the semester
    Returns None if no section can be scheduled
    '''
    for section in course.sections.values():
        time_available = True
        for a_class in section.classes:
            if time_conflict(a_class, semester):
                time_available = False
                break
        if time_available:
            return section
    return None


def time_conflict(a_class, semester):
    '''
    Returns True if the class to be scheduled conflicts with any class in the semester
    '''
    for section in semester.courses:
        for section_class in section.classes:
            if a_class.day == section_class.day:
                if overlap(a_class.start_time, a_class.duration, section_class.start_time, section_class.duration):
                    return True
    return False


def overlap(start1, duration1, start2, duration2):
    '''
    Returns True if the two time periods overlap
    '''
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
