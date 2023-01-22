'''
Currently just scrapes the course names and prereqs from the course catalog, with all "and/or" quantifiers for prereqs removed.

TODO:
- Add "and/or" quantifiers to prereqs
    - Modify the text parsing segmet (the body after of "prereq = body.split('Prerequisite(s)')") and CSV output so that it is no longer line by line, but instead has START and END columns for each course, allowing us to have multiple rows for courses with multiple prereq dependencies
'''

import time
from selenium import webdriver
from selenium.webdriver.common.by import By
import pandas as pd

MAX_PAGE = 18 # 18 pages of courses

driver = webdriver.Firefox(executable_path='/Users/matt/Documents/util/geckodriver')
# courses_url = 'https://catalog.mtroyal.ca/content.php?catoid=29&navoid=2305'
courses = {}

for page_num in range(1,MAX_PAGE+1):
    print(page_num)
    courses_url = 'https://catalog.mtroyal.ca/content.php?catoid=29&catoid=29&navoid=2305&filter[item_type]=3&filter[only_active]=1&filter[3]=1&filter[cpage]=' + str(page_num) + '#acalog_template_course_filter'
    driver.get(courses_url) # Opens the browser
    time.sleep(5)
    outer_table = driver.find_element(By.CSS_SELECTOR, 'td.block_content')
    mytable = outer_table.find_elements(By.CSS_SELECTOR, 'table.table_default')[2]

    for row in mytable.find_elements(By.CSS_SELECTOR, 'tr'):
        for cell in row.find_elements(By.CSS_SELECTOR, 'td'):
            if cell.text[0] == '•':
                # print(cell.text)
                course = cell.text[3:3+9]
                courses[course] = []
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
                            prereq = prereq[1].split(' ')
                            prereq_list = []
                            for index, segment in enumerate(prereq):
                                if segment.isupper() and prereq[index+1][:4].isnumeric():
                                    req = segment + ' ' + prereq[index+1][:4]
                                    prereq_list.append(req)
                            print(link_text,'\n',prereq_list,'\n\n')
                            courses[course] = prereq_list

                # time.sleep(1)
                # driver.find_element(By.LINK_TEXT, link_text).click()
            except:
                info = None
                # time.sleep(1)
                # driver.find_element(By.LINK_TEXT, link_text).click()
                pass

# print(courses)

driver.quit() # Closes the browser

df = pd.DataFrame.from_dict(courses, orient='index')
df.to_csv('courses.csv')