-- Landon Odishaw-Dyck (mid-arc)

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Move guards forward" #-}

import Data.List (intercalate)
import System.IO
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics


-- When making program list comprehension, make a function that checks if course is already scheduled in previous semester. 
-- Does not handle tutorials/labs or multiple sections offered per course. (change name to CRN)?
-- add courses taken in previous semesters to coursesTaken as a parameter when calling list comprehension. e.g. = (courseTaken ++ get_previous_semester_course_list)

data Session s = Session { dayOfWeek :: Int, start :: Int, duration :: Int }                             deriving (Show)

data Course c = Course { name :: String, prereqs :: [String], sessions :: [Session(Int,Int,Int)] }       deriving (Show)


data Semester s = Semester { courses :: [Course(String, [String], [Session(Int,Int,Int)])] }             deriving (Show)

data Program p = Program { semesters :: [Semester[Course(String, [String], [Session(Int,Int,Int)])]] } deriving (Show)

data Student = Student  { studName :: String, taken :: [String], required :: [String], coursesPerSem :: Int }                deriving (Show, Generic)

instance FromJSON Student

printStudent :: Student -> IO ()
printStudent student = do
  putStr ("Student Name: " ++ studName student)
  putStr "    Courses Taken: "
  putStr $ intercalate ", " (taken student)
  putStr "    Courses Required: "
  putStr $ intercalate ", " (required student)

printSession :: Session(Int,Int,int) -> IO ()
printSession session = putStrLn ("Day of Week: " ++ show (dayOfWeek session) ++ ", Start: " ++ show (start session) ++ ", Duration: " ++ show (duration session))

printSessionList :: [Session(Int,Int,int)] -> IO ()
printSessionList (x:xs) = do
  printSession x
  printSessionList xs
printSessionList [] = return ()

printCourse :: Course(String, [String], [Session(Int,Int,Int)]) -> IO ()
printCourse course = do
  putStr ("Course Name: " ++ name course)
  putStr "  Prequesites: "
  putStr $ intercalate ", " (prereqs course)
  putStr "  Sessions: "
  printSessionList (sessions course)

printCourseList :: [Course(String, [String], [Session(Int,Int,Int)])] -> IO ()
printCourseList (x:xs) = do
  printCourse x
  printCourseList xs
printCourseList [] = return ()


printSemester :: Semester[Course(String, [String], [Session(Int,Int,Int)])] -> IO ()
printSemester (Semester list) = printCourseList list

printSemesterList :: [Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> IO ()
printSemesterList (x:xs) = do
  putStrLn "Semester: "
  printSemester x
  printSemesterList xs
printSemesterList [] = return ()

printProgram :: Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]] -> IO ()
printProgram (Program list) = printSemesterList list

printProgramList :: [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]] -> IO ()
printProgramList (p:ps) = do
  putStrLn "Program: "
  printProgram p
  printProgramList ps
printProgramList [] = return ()


--In the 4 "contains" functions below, I should have used polymorphism, but ran out of time and patience to figure out the syntax.

-- Returns true if first param is in second
list_contains_string :: String -> [String] -> Bool
list_contains_string s = foldr (\ l -> (||) (s == l)) False

-- Returns true if an item in the first list is in the second list
any_string_in_list1_inside_list2 :: [String] -> [String] -> Bool
any_string_in_list1_inside_list2 (o:os) t = list_contains_string o t || any_string_in_list1_inside_list2 os t
any_string_in_list1_inside_list2 [] _ = False

-- Returns true if every String in first param list in is second param list
list_is_sublist :: [String] -> [String] -> Bool
list_is_sublist (o:os) (t:ts) = list_contains_string o (t:ts) && list_is_sublist os (t:ts)
-- list_is_sublist [] _ = True
-- list_is_sublist _ [] = False

list_is_sublist [] x = True
list_is_sublist [] (x:xs) = True
list_is_sublist x [] = False
list_is_sublist (x:xs) [] = False
list_is_sublist [] [] = True

-- Returns true if first param is in second (for course use)
course_list_contains_course :: Course(String,[String],[Session(Int,Int,Int)]) -> [Course(String,[String],[Session(Int,Int,Int)])] -> Bool
course_list_contains_course  s (l:ls) = sn == ln || course_list_contains_course  s ls
  where
    sn = get_name_from_course s
    ln = get_name_from_course l
course_list_contains_course  s [] = False

-- Returns true if every String in first param list in is second param list (for course use)
course_list_is_course_sublist :: [Course(String,[String],[Session(Int,Int,Int)])] -> [Course(String,[String],[Session(Int,Int,Int)])] -> Bool
course_list_is_course_sublist (o:os) (t:ts) = course_list_contains_course o (t:ts) && course_list_is_course_sublist os (t:ts)
course_list_is_course_sublist [] _ = True
course_list_is_course_sublist _ [] = False

--  Checks single against single session. Returns true if two session overlap 
conflict_session_1to1 :: Session(Int,Int,Int) -> Session(Int,Int,Int) -> Bool
conflict_session_1to1 (Session dow1 st1 dur1) (Session dow2 st2 dur2) = dow1==dow2  && ( st1 <= st2 && st1 + dur1 > st2  ||  st2 <= st1 && st2 + dur2 > st1 )

-- Checks one session against list.
conflict_session_1tolist :: Session(Int,Int,Int) -> [Session(Int,Int,Int)] -> Bool
conflict_session_1tolist o = foldr ((||) . conflict_session_1to1 o) False

-- Checks list against list. 
conflict_session_listtolist :: [Session(Int,Int,Int)] -> [Session(Int,Int,Int)] -> Bool
conflict_session_listtolist (o:os) (t:ts) =  conflict_session_1tolist o (t:ts) || conflict_session_listtolist os (t:ts)
conflict_session_listtolist [] (t:ts) = False

-- Returns true if any of sessions conflict
conflict_course_1to1 :: Course(String,[String],[Session(Int,Int,Int)]) -> Course(String,[String],[Session(Int,Int,Int)])  -> Bool
conflict_course_1to1 (Course _ _ (o:os) ) (Course _ _ (t:ts) ) = conflict_session_listtolist (o:os) (t:ts)
conflict_course_1to1 (Course _ _ [] ) (Course _ _ (t:ts) ) = False
conflict_course_1to1 (Course _ _ (o:os) ) (Course _ _ [] ) = False

-- Returns true if any courses conflict
conflict_course_1tolist :: Course(String,[String],[Session(Int,Int,Int)]) -> [Course(String,[String],[Session(Int,Int,Int)])]  -> Bool
conflict_course_1tolist o = foldr ((||) . conflict_course_1to1 o) False

-- Given Course, returns name
get_name_from_course :: Course(String,[String],[Session(Int,Int,Int)]) -> String
get_name_from_course (Course n _ _ ) = n

get_names_from_course_list :: [Course(String,[String],[Session(Int,Int,Int)])] -> [String]
get_names_from_course_list = map get_name_from_course

-- Given Course, returns prereqs
get_prereqs_from_course :: Course(String,[String],[Session(Int,Int,Int)]) -> [String]
get_prereqs_from_course (Course _ p _ ) = p

get_prereqs_from_course_list :: [Course(String,[String],[Session(Int,Int,Int)])] -> [String]
get_prereqs_from_course_list (c:cs) = remove_duplicates_from_string_list ( get_prereqs_from_course c ++ get_prereqs_from_course_list cs )
get_prereqs_from_course_list [] = []

remove_duplicates_from_string_list :: [String] -> [String]
remove_duplicates_from_string_list (x:xs) | list_contains_string x xs = remove_duplicates_from_string_list xs
                                          | otherwise = x:remove_duplicates_from_string_list xs
remove_duplicates_from_string_list x = x

-- Later: add fall/winter, data from csv for this function 
-- Given courseName and offeredCourses list, returns Course if offered, otherwise returns Course with name = "NA"
get_currently_offered_course :: String -> [Course(String,[String],[Session(Int,Int,Int)])] -> Course(String,[String],[Session(Int,Int,Int)])
get_currently_offered_course n [] = Course "NA" [] []
get_currently_offered_course n (c:cs) | name == n = c
                                      | name /= n = get_currently_offered_course n cs
    where name = get_name_from_course c

-- Given a list of semesters, removes all duplicate semesters.
-- use case: for each semester against every other one, check if its courses are subsets of one another, thus they are identical and one should be removed. 
remove_duplicates_from_course_list :: [Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> [Semester[Course(String, [String], [Session(Int,Int,Int)])]]
remove_duplicates_from_course_list (x:xs) | does_semester_occur_in_list x xs = remove_duplicates_from_course_list xs
                                          | otherwise = x:remove_duplicates_from_course_list xs
remove_duplicates_from_course_list x = x

remove_duplicates_from_program_list :: [Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]]]  -> [Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]]]
remove_duplicates_from_program_list (p:ps) | does_program_occur_in_list p ps = remove_duplicates_from_program_list ps
                                           | otherwise = p:remove_duplicates_from_program_list ps
remove_duplicates_from_program_list x = x

does_program_occur_in_list :: Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> [Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]]] -> Bool
does_program_occur_in_list p (x:xs) = all_course_names_from_program p == all_course_names_from_program x || does_program_occur_in_list p xs
does_program_occur_in_list p x = False

-- Checks if a semester occurs in a list of semesters.
does_semester_occur_in_list :: Semester[Course(String, [String], [Session(Int,Int,Int)])] -> [Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> Bool
does_semester_occur_in_list s (x:xs) = course_list_is_course_sublist sl xl && course_list_is_course_sublist xl sl || does_semester_occur_in_list s xs
    where
      sl = get_courses_from_semester s
      xl = get_courses_from_semester x
does_semester_occur_in_list s x = False

-- Place course at the front of the Course list of a Semester
course_into_semester :: Course(String,[String],[Session(Int,Int,Int)]) -> Semester[Course(String, [String], [Session(Int,Int,Int)])] -> Semester[Course(String, [String], [Session(Int,Int,Int)])]
course_into_semester c (Semester cs ) = Semester (c:cs)

-- Place semester at the front of the semester list of a program
semester_into_program :: Semester[Course(String, [String], [Session(Int,Int,Int)])] -> Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]]
semester_into_program x (Program xs) = Program (x:xs)

-- Given Semester, returns the list of courses within.
get_courses_from_semester :: Semester[Course(String, [String], [Session(Int,Int,Int)])] -> [Course(String, [String], [Session(Int,Int,Int)])]
get_courses_from_semester (Semester l) = l

get_semesters_from_program :: Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> [Semester[Course(String, [String], [Session(Int,Int,Int)])]]
get_semesters_from_program (Program s) = s

--Returns list of all prereqs (names) of courses in a semester. (removes duplicates)
all_prereq_names_from_semester :: Semester[Course(String, [String], [Session(Int,Int,Int)])] -> [String]
all_prereq_names_from_semester s =  remove_duplicates_from_string_list (get_prereqs_from_course_list c)
  where c = get_courses_from_semester s

--Returns list of all course (names) in a semester. (does not remove duplicates)
all_course_names_from_semester :: Semester[Course(String, [String], [Session(Int,Int,Int)])] -> [String]
all_course_names_from_semester s = get_names_from_course_list c
  where c = get_courses_from_semester s

--Returns list of all courses(names) from list of semesters. (does not remove duplicates)
all_course_names_from_semester_list :: [Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> [String]
all_course_names_from_semester_list = concatMap all_course_names_from_semester

--function that makes list of all courses (names) in a Program. (does not remove duplicates)
all_course_names_from_program :: Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> [String]
all_course_names_from_program p = all_course_names_from_semester_list s
  where s = get_semesters_from_program p

is_empty :: [a] -> Bool
is_empty (x:xs) = False
is_empty [] = True

takeProgram :: Int -> [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]] -> [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]]
takeProgram 0 _      = []
takeProgram _ []     = []
takeProgram n (x:xs) = x : take (n-1) xs


------ schedule_1course_to_semester
-- Attempts to add 1 course from student's required course list. 
-- If it can add, it adds it and adds the resulting Semester permutation to the returned Semester list. 
-- Goes through entire required list for each Semester of the list Semesters it is given.

--                               List of possible semesters, from previous run.                   currently offered courses                             Given student                        list of all permutations of semesters with one more course added
schedule_1course_to_semester :: [Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> [Course(String, [String], [Session(Int,Int,Int)])] -> Student -> [Semester[Course(String, [String], [Session(Int,Int,Int)])]]
-- schedule_1course_to_semester possibleSemesterSchedules coursesOffered student = remove_duplicates_from_course_list newPerms
schedule_1course_to_semester possibleSemesterSchedules coursesOffered student = newPerms

  where newPerms = [ if canAddNewCourse then course_into_semester c s else s |  s <- possibleSemesterSchedules,      x <- required student,
  -- where newPerms = take 1 [ if canAddNewCourse then course_into_semester c s else s |  s <- possibleSemesterSchedules,      x <- required student,

          let c = get_currently_offered_course x coursesOffered,
          let n = get_name_from_course c,
          let p = get_prereqs_from_course c,
          let l = get_courses_from_semester s,

          let canAddNewCourse = n /= "NA" &&
                                  not ( list_contains_string x (taken student) ) &&        --not taken by student
                                  --(list_is_sublist p (taken student)) &&                  --student has prereqs (relic)   
                                  not (conflict_course_1tolist c l )            ]        --no conflict with existing schedule

-- schedule_course_recursive ::

-- create full semester        (When called, Seed must be 1 empty Semester)
--                    CoursePerSem  seed                                                              offered courses                                     given student                        
createFullSemester :: Int      ->   [Semester[Course(String, [String], [Session(Int,Int,Int)])]] -> [Course(String, [String], [Session(Int,Int,Int)])] -> Student -> [Semester[Course(String, [String], [Session(Int,Int,Int)])]]
createFullSemester n seed offered student | n-1 == 0 = schedule_1course_to_semester seed offered student
                                          | n-1 > 0 =  schedule_1course_to_semester recursive offered student
  where recursive = createFullSemester (n-1) seed offered student


--                               list of possible programs, from prev run                                   coursesPerSem  offered courses                                        given student                        list of all permutations of programs with one more semester added
schedule_1semester_to_program :: [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]] -> Int     ->     [Course(String, [String], [Session(Int,Int,Int)])]  -> Student -> [Program[Semester[Course(String, [String], [Session(Int,Int,Int)])]]]
schedule_1semester_to_program  possibleProgramSchedules coursesPerSem coursesOffered student = remove_duplicates_from_program_list newPerms

  where newPerms = [ if canAddNewSemester then semester_into_program s p else p |  p <- possibleProgramSchedules,    s <- createFullSemester coursesPerSem [Semester []] coursesOffered student,

          let prevTaken = all_course_names_from_program p ++ taken student,
          let newSem = all_course_names_from_semester s,
          let prereqs = all_prereq_names_from_semester s,

          let canAddNewSemester = list_is_sublist prereqs prevTaken &&                             -- do courses in new semester have their prereqs taken already?  i.e. all prereqs for courses in semester s exist in given program p or student.taken
                                    not ( any_string_in_list1_inside_list2 newSem prevTaken )   ]    --not retaking any courses that already taken? 

--create full program ( seed must be list of one empty program ). Pass whichever semester is the upcoming semester into the 4th parameter, and the semester after that into the 5th parameter. The variable names are assuming fall is upcoming.
--                                                   seed 
createFullProgram :: Int      ->  Int        ->     [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]]  -> [Course(String, [String], [Session(Int,Int,Int)])] -> [Course(String, [String], [Session(Int,Int,Int)])] -> Student-> [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]]
createFullProgram  numOfSems  coursePerSem  seed  offeredFall offeredWinter  student | check = seed
                                                                                     | numOfSems-1 == 0 && odd numOfSems = schedule_1semester_to_program seed coursePerSem offeredFall student
                                                                                     | numOfSems-1 == 0 && even numOfSems = schedule_1semester_to_program seed coursePerSem offeredWinter student
                                                                                     | numOfSems-1 > 0 && odd numOfSems = schedule_1semester_to_program recursive coursePerSem offeredFall student
                                                                                     | numOfSems-1 > 0 && even numOfSems = schedule_1semester_to_program recursive coursePerSem offeredWinter student
  where 
    recursive = createFullProgram (numOfSems-1) coursePerSem seed offeredFall offeredWinter student
    check = list_is_sublist  (required student)  (all_course_names_from_program (head seed)++taken student)

-- increments the semester count until a program that graduates is found. First param must be 1. 
find_shortest_programs :: Int -> Int -> [Course(String,[String],[Session(Int,Int,Int)])] -> [Course(String,[String],[Session(Int,Int,Int)])] -> Student -> [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]]
find_shortest_programs numOfSems coursePerSem coursesOfferedFall coursesOfferedWinter student | numOfSems == 0 = []
                                                                                              | is_empty filteredPrograms = find_shortest_programs (numOfSems-1) coursePerSem coursesOfferedFall coursesOfferedWinter student
                                                                                              | otherwise = filteredPrograms
  where
    allPrograms = createFullProgram numOfSems coursePerSem [Program []] coursesOfferedFall coursesOfferedWinter student
    filteredPrograms = filter_programs_that_graduate allPrograms student

-- Returns list of programs that fulfill the required courses                                                
filter_programs_that_graduate :: [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]] -> Student  -> [Program[Semester[Course(String,[String],[Session(Int,Int,Int)])]]]
filter_programs_that_graduate (p:ps) student | check = p:filter_programs_that_graduate ps student
                                              | otherwise = filter_programs_that_graduate ps student
  where check = list_is_sublist  (required student)  (all_course_names_from_program p++taken student)
filter_programs_that_graduate [] _ = []

coursesOfferedFall = [Course "COMP1631" [] [Session 1 11 1, Session 3 11 1, Session 5 11 1],
  Course "COMP2613" ["MATH1271", "COMP1633"] [Session 2 14 1, Session 4 14 1],
  Course "COMP2631" ["COMP1633"] [Session 2 10 1, Session 4 10 1],
  Course "COMP2655" ["COMP1633"] [Session 1 14 1, Session 3 14 1, Session 5 14 1],
  Course "COMP3309" [] [Session 2 10 1, Session 4 10 1],
  Course "COMP3659" ["COMP2631","COMP2659"] [Session 1 8 1, Session 3 8 1, Session 5 8 1],
  Course "MATH1200" [] [Session 2 13 1, Session 4 13 1],
  Course "MATH1203" [] [Session 1 16 1, Session 3 16 1, Session 5 16 1],
  Course "MATH1271" ["MATH1203"] [Session 1 8 1, Session 3 8 1, Session 5 8 1],
  Course "COMP2521" ["COMP1631"] [Session 2 2 1, Session 4 2 1],
  Course "COMP3505" ["COMP1633","COMP2631"] [Session 1 7 1, Session 3 7 1, Session 5 7 1],
  Course "COMP3533" ["COMP2625"] [Session 1 4 1, Session 3 4 1, Session 5 4 1],
  Course "COMP4513" ["COMP3612"] [Session 1 9 1, Session 3 9 1, Session 5 9 1],
  Course "COMP4522" ["COMP2521"] [Session 1 11 1, Session 3 11 1, Session 5 11 1],
  Course "COMP4630" ["COMP2533"] [Session 1 12 1, Session 3 12 1, Session 5 12 1],
  Course "COMP4635" ["COMP3533"] [Session 2 25 1],
  Course "COMP5690" ["COMP2659"] [Session 0 25 1]]


coursesOfferedWinter = [Course "COMP1633" ["COMP1631"] [Session 1 10 1, Session 3 10 1, Session 5 10 1],
  Course "COMP2633" ["COMP2631"] [Session 2 8 1, Session 4 8 1],
  Course "COMP2659" ["PHIL1179","COMP2655"] [Session 1 15 1, Session 3 15 1, Session 5 15 1],
  Course "COMP3309" [] [Session 2 11 1, Session 4 11 1],
  Course "COMP3614" ["COMP2631","COMP2613"] [Session 2 14 1, Session 4 14 1],
  Course "COMP3649" ["COMP2613","COMP2631","PHIL1179"] [Session 2 16 1, Session 4 16 1],
  Course "MATH1271" ["COMP1203"] [Session 1 8 1, Session 3 8 1, Session 5 8 1],
  Course "MATH2234" ["MATH1200"] [Session 1 10 1, Session 3 10 1, Session 5 10 1],
  Course "PHIL1179" [] [Session 1 16 1, Session 3 16 1, Session 5 16 1],
  Course "COMP3533" ["COMP2655"] [Session 1 5 1, Session 3 5 1, Session 5 5 1],
  Course "COMP3612" ["COMP1633"] [Session 1 3 1, Session 3 3 1, Session 5 3 1],
  Course "COMP3625" ["COMP2631","COMP2613"] [Session 2 6 1, Session 4 6 1],
  Course "COMP4555" ["COMP2659"] [Session 1 8 1, Session 3 8 1, Session 5 8 1],
  Course "COMP5690" ["COMP2659"] [Session 0 25 1]]
    --prints bottom up schedules. 
    --must offer courses that are required or it hangs (fix list)

main :: IO ()
main = do

  -- taken = (take 1 shortestPrograms)

  jsonAlice <- BSL.readFile "test_json/Alice.json"

  case decode jsonAlice :: Maybe Student of
    Just student -> printProgramList (take 1 ( find_shortest_programs 5 (coursesPerSem student) coursesOfferedFall coursesOfferedWinter student ) )
    Nothing -> putStrLn "Failed to parse JSON"

