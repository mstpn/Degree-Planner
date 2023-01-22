'''
Scrapes the courses, adds each as its own Course object, and then adds the prereqs to each course object (NOT linked to other courses yet)

TODO:
    - Have the objects link to each other right from the start
        - make courses a dict of {course_name: Course}
            - for each course, if course doesn't exist, create it, else get it from dict
            - for each prereq, if course doesn't exist, create it, else get it from dict
'''

# import pickle
import csv
import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from class_definitions import Course, IGNORE_COURSES

# MAX_PAGE = 18 # 18 pages of courses
MAX_PAGE = 1

driver = webdriver.Firefox(executable_path='/Users/matt/Documents/util/geckodriver')
# courses_url = 'https://catalog.mtroyal.ca/content.php?catoid=29&navoid=2305'
courses = []

for page_num in range(1,MAX_PAGE+1):
    print(page_num)
    # courses_url = 'https://catalog.mtroyal.ca/content.php?catoid=29&catoid=29&navoid=2305&filter[item_type]=3&filter[only_active]=1&filter[3]=1&filter[cpage]=' + str(page_num) + '#acalog_template_course_filter'
    courses_url = 'https://catalog.mtroyal.ca/content.php?filter%5B27%5D=-1&filter%5B29%5D=&filter%5Bcourse_type%5D=5448&filter%5Bkeyword%5D=&filter%5B32%5D=1&filter%5Bcpage%5D=1&cur_cat_oid=29&expand=&navoid=2305&search_database=Filter&filter%5Bexact_match%5D=1#acalog_template_course_filter'
    driver.get(courses_url) # Opens the browser
    time.sleep(5)
    outer_table = driver.find_element(By.CSS_SELECTOR, 'td.block_content')
    mytable = outer_table.find_elements(By.CSS_SELECTOR, 'table.table_default')[2]

    for row in mytable.find_elements(By.CSS_SELECTOR, 'tr'):
        for cell in row.find_elements(By.CSS_SELECTOR, 'td'):
            if cell.text[0] == '•':
                # print(cell.text)
                course = cell.text[3:3+9]
                courses.append(Course(course))
                pass
            else:
                continue
            try:
                link_text = cell.text[3:]
                if len(link_text) < 3:
                    continue
                driver.find_element(By.LINK_TEXT, link_text).click()
                time.sleep(0.5)
                inner_table = driver.find_elements(By.CSS_SELECTOR, 'td.coursepadding')[-1]
                info = inner_table.find_elements(By.CSS_SELECTOR, 'div')
                if info:
                    for div in info:
                        body = div.text
                        if len(body) <5:
                            continue
                        prereq = body.split('Prerequisite(s)')
                        if len(prereq) > 1:
                            if course == 'MATH 1200' or course == 'MATH 3101':
                                x=1
                                pass
                            prereq = prereq[1].split('Note:')[0]
                            prereq = prereq.split('and')
                            for req_index, required in enumerate(prereq):
                                required = required.split(' ')
                                prereq_list = []
                                for seg_index, segment in enumerate(required):
                                    if segment.isupper() and required[seg_index+1][:4].isnumeric():
                                        req = segment + ' ' + required[seg_index+1][:4]
                                        prereq_list.append(req)
                                if len(prereq_list) > 0:
                                    courses[-1].add_prereq_list(prereq_list)

                # time.sleep(1)
                # driver.find_element(By.LINK_TEXT, link_text).click()
            except:
                info = None
                # time.sleep(1)
                # driver.find_element(By.LINK_TEXT, link_text).click()
                pass

# print(courses)

driver.quit() # Closes the browser

with open('math_courses.csv', 'w') as f:
    writer = csv.writer(f)
    for course in courses:
        writer.writerow(['START'])
        writer.writerow([course.name])
        for prereq_list in course.prereqs:
            writer.writerow(prereq_list)
        writer.writerow(['END'])

# with open('math_courses.pkl', 'wb') as f:
#     pickle.dump(courses, f)

# df = pd.DataFrame.from_dict(courses, orient='index')
# df.to_csv('math_courses.csv')