import class_definitions as cd
import student as std
from class_definitions import F, W


def create_schedule(stud, course_dict):
    all_courses = stud.all_required_courses()
    courses_left = all_courses.copy()

    graph_nodes_list, graph_nodes_dict = stud.make_graph()

    program = cd.Registration()
    num_semesters = 0
    year = 1
    semester_worf = stud.semester

    while len(all_courses) > 0 and year <= stud.years_to_grad:
        semester_object = cd.Semester(
            year, semester_worf, stud.max_courses_per_semester
        )

        semester_full = schedule_semester(
            courses_left,
            course_dict,
            graph_nodes_dict,
            semester_object,
            program,
        )
        if not semester_full:
            #! right now we have all possible core/required courses scheduled, but still room for electives
            # TODO schedule_gned()
            # -> schedule_gned() will need courses to specify what GNED they specify
            # ->-> requires catalog_scraper update
            pass
        num_semesters += 1
        program.semesters.append(semester_object)
        remove_scheduled_courses(all_courses, semester_object)

        # prep for next semester
        courses_left = all_courses.copy()
        # print the semester
        # print(semester_object)
        if semester_worf == F:
            semester_worf = W
        else:
            semester_worf = F
        if num_semesters % 2 == 0:
            year += 1

    program.years = program.get_years()
    program.num_semesters = len(program.semesters)

    return program


def remove_scheduled_courses(all_courses, semester_object):
    for sections in semester_object.courses:
        try:
            all_courses.remove(sections.course_name)
        except:
            print(sections.course_name), " not in all_courses"
    return


def schedule_semester(
    all_courses, course_dict, graph_nodes_dict, semester, program
):
    if len(all_courses) == 0:
        return False
    next_courses = all_courses.copy()
    max_courses_per_semester = semester.max_courses

    schedule_slots_available = True

    while (
        len(semester.courses) < max_courses_per_semester
        and schedule_slots_available
        and len(all_courses) > 0
    ):
        course_name = all_courses.pop(0)
        course_node = graph_nodes_dict.get(course_name, None)
        has_prereqs = True
        if course_node and not course_node.taken:
            for prereq in course_node.pre:
                if not prereq.taken:
                    has_prereqs = False
                    break
            if not has_prereqs:
                continue
            course_details = course_dict[semester.worf].get(course_name, None)
            if course_details is None:
                continue
            if schedule_course(course_details, semester):
                course_node.taken = True
                continue
            else:
                schedule_slots_available = schedule_semester(
                    all_courses,
                    course_dict,
                    graph_nodes_dict,
                    semester,
                    program,
                )
        if not course_node.taken:
            all_courses.append(course_name)
    if (
        len(semester.courses) < semester.max_courses
        or schedule_slots_available is False
    ):
        return False  # still course slots available to schedule

    return True  # all courses scheduled


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
    Returns a section of a course that can be scheduled in the semester
    Returns None if no section can be scheduled
    """
    cant_scheudule = -1
    no_sections = -2
    new_section = None

    for section in course.sections.values():
        time_available = True
        types = [cant_scheudule, cant_scheudule, cant_scheudule]
        for i in range(len(section.class_types)):
            if len(section.class_types[i]) == 0:
                types[i] = no_sections
                continue
            for type_section in section.class_types[i].values():
                section_available = True
                for a_class in type_section:
                    if time_conflict(a_class, semester):
                        section_available = False
                        break
                if section_available:
                    types[i] = type_section[0].id
                    break
        if cant_scheudule in types:
            time_available = False
            break
        if time_available:
            new_section = cd.Section(
                section.course_name, section.id, section.description
            )
            for index, value in enumerate(types):
                if value != no_sections:
                    new_section.class_types[index][
                        value
                    ] = section.class_types[index][value]
            return new_section
    return None


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