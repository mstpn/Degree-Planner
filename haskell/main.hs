-- Program that schedules one semester (week) of courses.
-- Landon Odishaw-Dyck (mid-arc)


-- When making program list comprehension, make a function that checks if course is already scheduled in previous semester. 
  
-- Does not handle tutorials/labs or multiple sections offered per course. (change name to CRN)?
-- add courses taken in previous semesters to coursesTaken as a parameter when calling list comprehension. e.g. = (courseTaken ++ get_previous_semester_course_list)
  


data Session s = Session { dayOfWeek :: Int, start :: Int, duration :: Int }                             deriving (Show) 

                                                                                  day start duration
data Course c = Course { name :: String, prereqs :: [String], sessions :: [Session(Int,Int,Int)] }       deriving (Show) 

data Student s = Student  { studName :: String, taken :: [String], required :: [String] } | Nothing      deriving (Show) 

data Semester s = Semester { courses :: [Course(String, [String], [Session(Int,Int,Int)])] }             deriving (Show) 


--not finished
printStudent :: Student(String,[String],[String]) -> IO ()
printStudent student = do
  putStrLn ("Name")

--not finished
printSemester :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> IO ()
printSemester semester = do
  putStrLn (" test" )


--In the 4 "contains" functions below, I should have used polymorphism, but ran out of time and patience to figure out the syntax.


-- Returns true if first param is in second
list_contains_string :: String -> [String] -> Bool
list_contains_string s (l:ls) = s == l || (list_contains_string s ls )
list_contains_string s [] = False

-- Returns true if every String in first param list in is second param list
list_is_sublist :: [String] -> [String] -> Bool
list_is_sublist (o:os) (t:ts) = ( list_contains_string o (t:ts) ) && ( list_is_sublist os (t:ts) )
list_is_sublist [] _ = True


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

-- Given Course, returns prereqs
get_prereqs_from_course :: Course(String,[String],[Session(Int,Int,Int)]) -> [String]
get_prereqs_from_course (Course _ p _ ) = p

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

-- Checks if a semester occurs in a list of semesters.
does_semester_occur_in_list :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> Bool
does_semester_occur_in_list s (x:xs) = ( (course_list_is_course_sublist sl xl) && (course_list_is_course_sublist xl sl) ) || (does_semester_occur_in_list s xs)
    where
      sl = ( get_courses_from_semester s )
      xl = ( get_courses_from_semester x )
does_semester_occur_in_list s x = False

-- Place course into a the Course list of a Semester
course_into_semester :: Course(String,[String],[Session(Int,Int,Int)]) -> Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> Semester([Course(String, [String], [Session(Int,Int,Int)])])
course_into_semester c (Semester cs ) = Semester (c:cs)

-- Given Semester, returns the list of courses within.
get_courses_from_semester :: Semester([Course(String, [String], [Session(Int,Int,Int)])]) -> [Course(String, [String], [Session(Int,Int,Int)])]
get_courses_from_semester (Semester l) = l

------ schedule_1course_to_semester
-- Attempts to add 1 course from student's required course list. 
-- If it can add, it adds it and adds the resulting Semester permutation to the returned Semester list. 
-- Goes through entire required list for each Semester of the list Semesters it is given.

--                               List of possible semesters, from previous run.                   currently offered courses                             Given student                        New semester permutation list
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
                                  not(conflict_course_1tolist c l ) )            ]        --no conflict with existing schedule (checks previously scheduled course due to time conflict. Could add different check to compare courseNames.)

-- create full semester
--when called, third param must be zero.  CurrIter must be empty an empty Semester. 
--                    courseCount seed                                                              offered courses                                     given student                        
createFullSemester :: Int   ->    [Semester([Course(String, [String], [Session(Int,Int,Int)])])] -> [Course(String, [String], [Session(Int,Int,Int)])] -> Student(String,[String],[String]) -> [Semester([Course(String, [String], [Session(Int,Int,Int)])])]
createFullSemester n seed offered student | n == 0 = (schedule_1course_to_semester seed offered student)
createFullSemester n seed offered student | n > 0 =  (schedule_1course_to_semester recursive offered student)
  where recursive = (createFullSemester (n-1) seed offered student)


coursesOffered = [ Course "CourseA" [] [ Session 0 2 1 ],
                    Course "CourseB" ["CourseA"] [ Session 0 7 1 ],
                    Course "CourseC" [] [ Session 0 3 1 ] ]


main = do
  let student = Student "El Chappo" [] ["CourseA","CourseB","CourseC"]

  let fullSemester = (createFullSemester 1 [(Semester [])] coursesOffered student  ) 

  print fullSemester
  











-- We must remove duplicates for two reasons. 
-- 1) Semester1[C1,C2] = Semester2[C2,C1], are the same, thus these must be removed.
-- 2) ... [if canAddNewPermu then (course_into_semester c s) else s | ... The else part of the list-comp was added because when no more courses could be added to 
-- a given semester, we should just leave that item in the list, rather than only adding semesters that could have additional courses added on.
-- But this leads to duplicates being added in some cases. Example: We are given 1 previous schedule. And we are given 2 courses that still need adding, 
-- but that conflict with the existing schedule. Thus we will attempt and fail to add 2 different courses, leading
-- to 2 permutations (where 2 different were unable to insert into the semester), and are duplicates. 
-- To keep list comprehension readable, this we abstracted the duplicate prevention into its own function.

  



