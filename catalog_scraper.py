'''
Scrapes the courses, adds each as its own Course object, and then adds the prereqs to each course object (NOT linked to other courses yet)

TODO:
    - Have the objects link to each other right from the start
        - make courses a dict of {course_name: Course}
            - for each course, if course doesn't exist, create it, else get it from dict
            - for each prereq, if course doesn't exist, create it, else get it from dict
'''

import pickle
import csv
import time
import os
from selenium import webdriver
from selenium.webdriver.common.by import By
from class_definitions import Course, IGNORE_COURSES, IGNORE_PREREQS, IGNORE_DEPTS

MAX_PAGE = 18  # get this manually from catalog.mtroyal.ca

# TESTING (MATH courses only)
# courses_url = 'https://catalog.mtroyal.ca/content.php?filter%5B27%5D=-1&filter%5B29%5D=&filter%5Bcourse_type%5D=5448&filter%5Bkeyword%5D=&filter%5B32%5D=1&filter%5Bcpage%5D=1&cur_cat_oid=29&expand=&navoid=2305&search_database=Filter&filter%5Bexact_match%5D=1#acalog_template_course_filter'
# MAX_PAGE = 1

driver = webdriver.Firefox(
    executable_path='/Users/matt/Documents/util/geckodriver', service_log_path=os.devnull)
# courses_url = 'https://catalog.mtroyal.ca/content.php?catoid=29&navoid=2305'
courses = []
course_dict = {}

for page_num in range(1, MAX_PAGE+1):
    print(page_num)
    courses_url = 'https://catalog.mtroyal.ca/content.php?catoid=29&catoid=29&navoid=2305&filter[item_type]=3&filter[only_active]=1&filter[3]=1&filter[cpage]=' + str(
        page_num) + '#acalog_template_course_filter'

    driver.get(courses_url)  # Opens the browser
    time.sleep(5)
    outer_table = driver.find_element(By.CSS_SELECTOR, 'td.block_content')
    mytable = outer_table.find_elements(
        By.CSS_SELECTOR, 'table.table_default')[2]

    for row in mytable.find_elements(By.CSS_SELECTOR, 'tr'):
        for cell in row.find_elements(By.CSS_SELECTOR, 'td'):
            if cell.text[0] == '•':
                course = cell.text[3:3+9]
                course = course.replace(' ', '')
                # ! NEW
                if course in IGNORE_COURSES or course[:4] in IGNORE_DEPTS:
                    continue
                course_obj = course_dict.get(course, None)
                if course_obj is None:
                    course_obj = Course(course)
                    course_dict[course] = course_obj
                courses.append(course_obj)
                # ! End NEW

            else:
                continue
            try:
                link_text = cell.text[3:]
                if len(link_text) < 3:
                    continue
                driver.find_element(By.LINK_TEXT, link_text).click()
                time.sleep(0.5)
                inner_table = driver.find_elements(
                    By.CSS_SELECTOR, 'td.coursepadding')[-1]
                info = inner_table.find_elements(By.CSS_SELECTOR, 'div')
                if info:
                    for div in info:
                        body = div.text
                        if len(body) < 5:
                            continue
                        prereq = body.split('Prerequisite(s)')
                        if len(prereq) > 1:
                            prereq = prereq[1].split(':')[0]
                            prereq = prereq.split('and')
                            for req_index, required in enumerate(prereq):
                                required = required.split(' ')
                                prereq_list = []
                                for seg_index, segment in enumerate(required):
                                    if segment.isupper() and required[seg_index+1][:4].isnumeric():
                                        req = segment + \
                                            required[seg_index+1][:4]
                                        # ! NEW
                                        if req in IGNORE_PREREQS:
                                            continue
                                        req_obj = course_dict.get(req, None)
                                        if req_obj is None:
                                            req_obj = Course(req)
                                            course_dict[req] = req_obj
                                        prereq_list.append(req_obj)
                                        # ! End NEW
                                if len(prereq_list) > 0:
                                    course_obj.prereqs.append(prereq_list)
            except:
                info = None
                pass


driver.quit()  # Closes the browser

data_folder = 'data/'
course_file = data_folder + 'courses.csv'
print('Writing to file...')
with open(course_file, 'w') as f:
    writer = csv.writer(f)
    for course in courses:
        writer.writerow(['START'])
        writer.writerow([course.name])
        for prereq_list in course.prereqs:
            name_list = []
            for prereq in prereq_list:
                name_list.append(prereq.name)
            writer.writerow(name_list)
        writer.writerow(['END'])

pickle_folder = 'pickles/'
pickle_file = pickle_folder + 'courses_no_semester.pkl'
print('Writing to pickle...')
with open(pickle_file, 'wb') as f:
    pickle.dump(courses, f)

print('Done')
