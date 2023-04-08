-- Program that schedules one semester (week) of courses.
-- Landon Odishaw-Dyck (mid-arc)

import Data.List (intercalate)


-- When making program list comprehension, make a function that checks if course is already scheduled in previous semester. 
  
-- Does not handle tutorials/labs or multiple sections offered per course. (change name to CRN)?
-- add courses taken in previous semesters to coursesTaken as a parameter when calling list comprehension. e.g. = (courseTaken ++ get_previous_semester_course_list)
  


data Session s = Session { dayOfWeek :: Int, start :: Int, duration :: Int }                             deriving (Show) 

data Course c = Course { name :: String, prereqs :: [String], sessions :: [Session(Int,Int,Int)] }       deriving (Show) 

                                                                                                                          -- add these to student:  CoursesPerSemester, Number of semesters
data Student s = Student  { studName :: String, taken :: [String], required :: [String] } | Nothing      deriving (Show) 

data Semester s = Semester { courses :: [Course(String, [String], [Session(Int,Int,Int)])] }             deriving (Show) 

data Program p = Program { semesters :: [Semester([Course(String, [String], [Session(Int,Int,Int)])])] } deriving (Show)


printStudent :: Student(String,[String],[Session(Int,Int,Int)]) -> IO ()
printStudent student = do
  putStr ("Student Name: " ++ studName student)
  putStr ("    Courses Taken: ")
  putStr $ intercalate ", " (taken student)
  putStr ("    Courses Required: ")
  putStr $ intercalate ", " (required student)

printSession :: Session(Int,Int,int) -> IO ()
printSession session = do
    putStrLn ("Day of Week: " ++ show (dayOfWeek session) ++ ", Start: " ++ show (start session) ++ ", Duration: " ++ show (duration session))

printSessionList :: [Session(Int,Int,int)] -> IO ()
printSessionList (x:xs) = do
  printSession x 
  printSessionList xs
printSessionList [] = return ()

printCourse :: Course(String, [String], [Session(Int,Int,Int)]) -> IO ()
printCourse course = do 
  putStr ("Course Name: " ++ name course)
  putStr ("  Prequesites: ")
  putStr $ intercalate ", " (prereqs course)
  putStr ("  Sessions: ")
  printSessionList (sessions course)

printCourseList :: [Course(String, [String], [Session(Int,Int,Int)])] -> IO ()
printCourseList (x:xs) = do
  printCourse x 
  printCourseList xs
printCourseList [] = return ()


printSemester :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> IO ()
printSemester (Semester list) = do
  printCourseList list

printSemesterList :: [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> IO ()
printSemesterList (x:xs) = do
  putStrLn("Semester: ")
  printSemester x 
  printSemesterList xs
printSemesterList [] = return ()

printProgram :: Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])]) -> IO ()
printProgram (Program list) = do
  printSemesterList list

printProgramList :: [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])] -> IO ()
printProgramList (p:ps) = do  
  putStrLn("Program: ")
  printProgram p 
  printProgramList ps
printProgramList [] = return ()


--In the 4 "contains" functions below, I should have used polymorphism, but ran out of time and patience to figure out the syntax.

-- Returns true if first param is in second
list_contains_string :: String -> [String] -> Bool
list_contains_string s (l:ls) = s == l || (list_contains_string s ls )
list_contains_string s [] = False

-- Returns true if an item in the first list is in the second list
any_string_in_list1_inside_list2 :: [String] -> [String] -> Bool
any_string_in_list1_inside_list2 (o:os) t = (list_contains_string o t) || (any_string_in_list1_inside_list2 os t) 
any_string_in_list1_inside_list2 [] _ = False

-- Returns true if every String in first param list in is second param list
list_is_sublist :: [String] -> [String] -> Bool
list_is_sublist (o:os) (t:ts) = ( list_contains_string o (t:ts) ) && ( list_is_sublist os (t:ts) )
list_is_sublist [] _ = True
list_is_sublist _ [] = False

-- Returns true if first param is in second (for course use)
course_list_contains_course :: Course(String,[String],[Session(Int,Int,Int)]) -> [Course(String,[String],[Session(Int,Int,Int)])] -> Bool
course_list_contains_course  s (l:ls) = sn == ln || (course_list_contains_course  s ls )
  where
    sn = get_name_from_course s
    ln = get_name_from_course l
course_list_contains_course  s [] = False

-- Returns true if every String in first param list in is second param list (for course use)
course_list_is_course_sublist :: [Course(String,[String],[Session(Int,Int,Int)])] -> [Course(String,[String],[Session(Int,Int,Int)])] -> Bool
course_list_is_course_sublist (o:os) (t:ts) = ( course_list_contains_course o (t:ts) ) && ( course_list_is_course_sublist os (t:ts) )
course_list_is_course_sublist [] _ = True
course_list_is_course_sublist _ [] = False

--  Checks single against single session. Returns true if two session overlap 
conflict_session_1to1 :: Session(Int,Int,Int) -> Session(Int,Int,Int) -> Bool  
conflict_session_1to1 (Session dow1 st1 dur1) (Session dow2 st2 dur2) = dow1==dow2  && ( (st1 <= st2 && (st1 + dur1) > st2)  ||  (st2 <= st1 && (st2 + dur2) > st1) )

-- Checks one session against list.
conflict_session_1tolist :: Session(Int,Int,Int) -> [Session(Int,Int,Int)] -> Bool
conflict_session_1tolist o (t:ts) = (conflict_session_1to1 o t ) || (conflict_session_1tolist o ts )
conflict_session_1tolist o [] = False

-- Checks list against list. 
conflict_session_listtolist :: [Session(Int,Int,Int)] -> [Session(Int,Int,Int)] -> Bool
conflict_session_listtolist (o:os) (t:ts) =  (conflict_session_1tolist o (t:ts)) || (conflict_session_listtolist os (t:ts))
conflict_session_listtolist [] (t:ts) = False

-- Returns true if any of sessions conflict
conflict_course_1to1 :: Course(String,[String],[Session(Int,Int,Int)]) -> Course(String,[String],[Session(Int,Int,Int)])  -> Bool
conflict_course_1to1 (Course _ _ (o:os) ) (Course _ _ (t:ts) ) = conflict_session_listtolist (o:os) (t:ts)
conflict_course_1to1 (Course _ _ [] ) (Course _ _ (t:ts) ) = False
conflict_course_1to1 (Course _ _ (o:os) ) (Course _ _ [] ) = False

-- Returns true if any courses conflict
conflict_course_1tolist :: Course(String,[String],[Session(Int,Int,Int)]) -> [Course(String,[String],[Session(Int,Int,Int)])]  -> Bool
conflict_course_1tolist o (t:ts) = (conflict_course_1to1 o t) || conflict_course_1tolist o ts 
conflict_course_1tolist o [] = False

-- Given Course, returns name
get_name_from_course :: Course(String,[String],[Session(Int,Int,Int)]) -> String
get_name_from_course (Course n _ _ ) = n

get_names_from_course_list :: [Course(String,[String],[Session(Int,Int,Int)])] -> [String]
get_names_from_course_list (c:cs) = (get_name_from_course c):(get_names_from_course_list cs) 
get_names_from_course_list [] = []

-- Given Course, returns prereqs
get_prereqs_from_course :: Course(String,[String],[Session(Int,Int,Int)]) -> [String]
get_prereqs_from_course (Course _ p _ ) = p

get_prereqs_from_course_list :: [Course(String,[String],[Session(Int,Int,Int)])] -> [String]
get_prereqs_from_course_list (c:cs) = remove_duplicates_from_string_list( (get_prereqs_from_course c) ++ (get_prereqs_from_course_list cs) )
get_prereqs_from_course_list [] = [] 

remove_duplicates_from_string_list :: [String] -> [String]
remove_duplicates_from_string_list (x:xs) | (list_contains_string x xs) = (remove_duplicates_from_string_list xs)
                                          | otherwise = x:( remove_duplicates_from_string_list xs )
remove_duplicates_from_string_list x = x

-- Later: add fall/winter, data from csv for this function 
-- Given courseName and offeredCourses list, returns Course if offered, otherwise returns Course with name = "NA"
get_currently_offered_course :: String -> [Course(String,[String],[Session(Int,Int,Int)])] -> Course(String,[String],[Session(Int,Int,Int)])
get_currently_offered_course n [] = Course "NA" [] []
get_currently_offered_course n (c:cs) | name == n = c
                                      | name /= n = (get_currently_offered_course n cs)
    where name = get_name_from_course(c)

-- Given a list of semesters, removes all duplicate semesters.
-- use case: for each semester against every other one, check if its courses are subsets of one another, thus they are identical and one should be removed. 
remove_duplicates_from_course_list :: [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> [Semester([Course(String, [String], [Session(Int,Int,Int)])])]
remove_duplicates_from_course_list (x:xs) | ( does_semester_occur_in_list x xs ) = ( remove_duplicates_from_course_list xs )
                                          | otherwise = x:( remove_duplicates_from_course_list xs )
remove_duplicates_from_course_list x = x

remove_duplicates_from_program_list :: [Program([Semester([Course(String, [String], [Session(Int,Int,Int)])])])]  -> [Program([Semester([Course(String, [String], [Session(Int,Int,Int)])])])] 
remove_duplicates_from_program_list (p:ps) | ( does_program_occur_in_list p ps ) = ( remove_duplicates_from_program_list ps )
                                           | otherwise = p:( remove_duplicates_from_program_list ps )
remove_duplicates_from_program_list x = x                                    

does_program_occur_in_list :: Program([Semester([Course(String, [String], [Session(Int,Int,Int)])])]) -> [Program([Semester([Course(String, [String], [Session(Int,Int,Int)])])])] -> Bool
does_program_occur_in_list p (x:xs) = (  (all_course_names_from_program p) == (all_course_names_from_program x) || ( does_program_occur_in_list p xs )  )
does_program_occur_in_list p x = False

-- Checks if a semester occurs in a list of semesters.
does_semester_occur_in_list :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> Bool
does_semester_occur_in_list s (x:xs) = ( (course_list_is_course_sublist sl xl) && (course_list_is_course_sublist xl sl) ) || (does_semester_occur_in_list s xs)
    where
      sl = ( get_courses_from_semester s )
      xl = ( get_courses_from_semester x )
does_semester_occur_in_list s x = False

-- Place course at the front of the Course list of a Semester
course_into_semester :: Course(String,[String],[Session(Int,Int,Int)]) -> Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> Semester([Course(String, [String], [Session(Int,Int,Int)])])
course_into_semester c (Semester cs ) = Semester (c:cs)

-- Place semester at the front of the semester list of a program
semester_into_program :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> Program[Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> Program[Semester([Course(String, [String], [Session(Int,Int,Int)])])]
semester_into_program x (Program xs) = Program (x:xs)

-- Given Semester, returns the list of courses within.
get_courses_from_semester :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> [Course(String, [String], [Session(Int,Int,Int)])]
get_courses_from_semester (Semester l) = l

get_semesters_from_program :: Program[Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> [Semester([Course(String, [String], [Session(Int,Int,Int)])])]
get_semesters_from_program (Program s) = s

--Returns list of all prereqs (names) of courses in a semester. (removes duplicates)
all_prereq_names_from_semester :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> [String]
all_prereq_names_from_semester s =  (remove_duplicates_from_string_list (get_prereqs_from_course_list c) )
  where c = ( get_courses_from_semester s )

--Returns list of all course (names) in a semester. (does not remove duplicates)
all_course_names_from_semester :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> [String]
all_course_names_from_semester s = (get_names_from_course_list c )
  where c = ( get_courses_from_semester s )

--Returns list of all courses(names) from list of semesters. (does not remove duplicates)
all_course_names_from_semester_list :: [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> [String]
all_course_names_from_semester_list (x:xs) = (all_course_names_from_semester x ) ++ (all_course_names_from_semester_list xs)
all_course_names_from_semester_list [] = []

--function that makes list of all courses (names) in a Program. (does not remove duplicates)
all_course_names_from_program :: Program[Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> [String]
all_course_names_from_program p = (all_course_names_from_semester_list s)
  where s = (get_semesters_from_program p)

takeProgram :: Int -> [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])] -> [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])]
takeProgram 0 _      = []
takeProgram _ []     = []
takeProgram n (x:xs) = x : take (n-1) xs


------ schedule_1course_to_semester
-- Attempts to add 1 course from student's required course list. 
-- If it can add, it adds it and adds the resulting Semester permutation to the returned Semester list. 
-- Goes through entire required list for each Semester of the list Semesters it is given.

--                               List of possible semesters, from previous run.                   currently offered courses                             Given student                        list of all permutations of semesters with one more course added
schedule_1course_to_semester :: [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> [Course(String, [String], [Session(Int,Int,Int)])] -> Student(String,[String],[String]) -> [Semester([Course(String, [String], [Session(Int,Int,Int)])])]
schedule_1course_to_semester possibleSemesterSchedules coursesOffered student = (remove_duplicates_from_course_list newPerms) 
      
  where newPerms = [ if canAddNewCourse then (course_into_semester c s) else s |  s <- possibleSemesterSchedules,      x <- (required student),                  
  
          let c = ( get_currently_offered_course x coursesOffered ), 
          let n = ( get_name_from_course c ), 
          let p = ( get_prereqs_from_course c ), 
          let l = ( get_courses_from_semester s ),
                                       
          let canAddNewCourse = (      
                                  (n /= "NA") &&                                          --course is currently offered  
                                  not( list_contains_string x (taken student) ) &&        --not taken by student
                                  (list_is_sublist p (taken student)) &&                  --student has prereqs    
                                  not(conflict_course_1tolist c l ) )            ]        --no conflict with existing schedule

-- create full semester        (When called, Seed must be 1 empty Semester)
--                    CoursePerSem  seed                                                              offered courses                                     given student                        
createFullSemester :: Int      ->   [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> [Course(String, [String], [Session(Int,Int,Int)])] -> Student(String,[String],[String]) -> [Semester([Course(String, [String], [Session(Int,Int,Int)])])]
createFullSemester n seed offered student | n-1 == 0 = (schedule_1course_to_semester seed offered student)
createFullSemester n seed offered student | n-1 > 0 =  (schedule_1course_to_semester recursive offered student)
  where recursive = (createFullSemester (n-1) seed offered student)
  

--                               list of possible programs, from prev run                                   coursesPerSem  offered courses                                        given student                        list of all permutations of programs with one more semester added
schedule_1semester_to_program :: [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])] -> Int     ->     [Course(String, [String], [Session(Int,Int,Int)])]  -> Student(String,[String],[String]) -> [Program([Semester([Course(String, [String], [Session(Int,Int,Int)])])])] 
schedule_1semester_to_program  possibleProgramSchedules coursesPerSem coursesOffered student = (remove_duplicates_from_program_list newPerms)
      
  where newPerms = [ if canAddNewSemester then (semester_into_program s p) else p |  p <- possibleProgramSchedules,    s <- (createFullSemester coursesPerSem [(Semester [])] coursesOffered student),                

          let prevTaken = ( (all_course_names_from_program p) ++ (taken student) ),
          let newSem = (all_course_names_from_semester s),
          let prereqs = (all_prereq_names_from_semester s),

          let canAddNewSemester = (      
                                    (list_is_sublist prereqs prevTaken ) &&                             -- do courses in new semester have their prereqs taken already?  i.e. all prereqs for courses in semester s exist in given program p or student.taken
                                    not( (any_string_in_list1_inside_list2 newSem prevTaken) ) )   ]    --not retaking any courses that already taken? 
  

--create full program ( seed must be list of one empty program ). Pass whichever semester is the upcoming semester into the 4th parameter, and the semester after that into the 5th parameter. The variable names are assuming fall is upcoming.
--                                                   seed 
createFullProgram :: Int      ->  Int        ->     [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])]  -> [Course(String, [String], [Session(Int,Int,Int)])] -> [Course(String, [String], [Session(Int,Int,Int)])] -> Student(String,[String],[String]) -> [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])]
createFullProgram  numOfSems  coursePerSem  seed  offeredFall offeredWinter  student | (numOfSems-1 == 0) && (mod numOfSems 2 == 0) = (schedule_1semester_to_program seed coursePerSem offeredFall student )
                                                                                     | (numOfSems-1 == 0) && (mod numOfSems 2 /= 0) = (schedule_1semester_to_program seed coursePerSem offeredWinter student )
                                                                                     | (numOfSems-1 > 0) && (mod numOfSems 2 == 0) = (schedule_1semester_to_program recursive coursePerSem offeredFall student )
                                                                                     | (numOfSems-1 > 0) && (mod numOfSems 2 /= 0) = (schedule_1semester_to_program recursive coursePerSem offeredWinter student )
  where recursive = (createFullProgram (numOfSems-1) coursePerSem seed offeredFall offeredWinter student )

-- Returns list of programs that fulfill the required courses                                                req'd 
filter_programs_that_graduate :: [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])] -> [String] -> [Program([Semester([Course(String,[String],[Session(Int,Int,Int)])])])]
filter_programs_that_graduate (p:ps) required | check = p:(filter_programs_that_graduate ps required) 
                                              | otherwise = (filter_programs_that_graduate ps required) 
  where check = ( list_is_sublist required (all_course_names_from_program p) ) 
filter_programs_that_graduate [] _ = []




--assumptions: all courses offered in all semesters( rotate W/F for coursesOffered parameter in function that calls this function )
--givse as many courses as possible in given years, not always guaranteed to finish program reqs
-- assumes all courses offered for all sems (no w/f)
-- add courses to student, make copies (this might be better)
-- check prev semester courses 
-- change student to list of taken courses? 






coursesOffered = [ Course "CourseA" [] [ Session 0 2 1 ],
                    Course "CourseB" [] [ Session 0 7 1 ],
                    Course "CourseC" [] [ Session 0 3 1 ] ]

coursesOfferedFall = [ Course "FallA" [] [ Session 0 2 1 ],
                    Course "FallB" [] [ Session 0 7 1 ],
                    Course "FallC" [] [ Session 0 3 1 ] ]

coursesOfferedWinter = [ Course "WinterA" [] [ Session 0 2 1 ],
                    Course "WinterB" [] [ Session 0 7 1 ],
                    Course "WinterC" [] [ Session 0 3 1 ] ]


main = do
  let student = Student "El Chappo" [] ["WinterA","WinterB","WinterC","FallA","FallB","FallC"]


  let programList = ( createFullProgram 3 3 [(Program[])] coursesOfferedFall coursesOfferedWinter student )

  let graduationProgramsOnly = (filter_programs_that_graduate programList (required student))

  let taken = (take 1 graduationProgramsOnly)

  printProgramList graduationProgramsOnly
  









--Later optimization
-- make a function that sorts courses based on least amount of prereqs, how is that possible, I am only storing
-- string in the student class. It is only in the courses offered that have prereqs defined. 
-- Would we rearange the students field based on the coursesOffered? how does that work for fall/win?
-- Most amount of next-reqs. 
-- Has most conflicts, schedule first. 

-- make a function that takes first N fullest semesters. Is there any reason why not to take the fullest?
-- call it within createFullSemester.







-- We must remove duplicates for two reasons. 
-- 1) Semester1[C1,C2] = Semester2[C2,C1], are the same, thus these must be removed.
-- 2) ... [if canAddNewPermu then (course_into_semester c s) else s | ... The else part of the list-comp was added because when no more courses could be added to 
-- a given semester, we should just leave that item in the list, rather than only adding semesters that could have additional courses added on.
-- But this leads to duplicates being added in some cases. Example: We are given 1 previous schedule. And we are given 2 courses that still need adding, 
-- but that conflict with the existing schedule. Thus we will attempt and fail to add 2 different courses, leading
-- to 2 permutations (where 2 different were unable to insert into the semester), and are duplicates. 
-- To keep list comprehension readable, this we abstracted the duplicate prevention into its own function.

  



-- Decomposition of the different list comprehensions (semester and program) made it easier to read and code. But less efficient.  Wouldn't have learned without trying though.

-- but also, breaking up the compositions allowed me to cull bad semesters before moving onto the program phase. Limiting the search space.

-- It made it easier to tackle, by decomposing in semester construction, then into program construction.