-- Landon Odishaw-Dyck (mid-arc)


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Move guards forward" #-}
{-# HLINT ignore "Use null" #-}

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
  ( encodeDefaultOrderedByName
  , DefaultOrdered(headerOrder)
  , Header
  , namedRecord
  , ToNamedRecord(toNamedRecord)
  , (.=) 
  )

import qualified Data.Csv as CSV
import qualified Data.Foldable as Foldable
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, (\\))
import System.IO
import Data.Aeson hiding ((.=))
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics
import System.Environment

-- When making program list comprehension, make a function that checks if course is already scheduled in previous semester. 
-- Does not handle tutorials/labs or multiple sections offered per course. (change name to CRN)?
-- add courses taken in previous semesters to coursesTaken as a parameter when calling list comprehension. e.g. = (courseTaken ++ get_previous_semester_course_list)

-- data Session = Session { dayOfWeek :: Int, start :: Int, duration :: Int }                             deriving (Show)

-- data Course = Course { name :: String, prereqs :: [String], sessions :: [Session] }       deriving (Show)

-- data Semester = Semester { courses :: [Course] }             deriving (Show)

-- data Program = Program { semesters :: [Semester] } deriving (Show)

-- data Student = Student  { studName :: String, taken :: [String], required :: [String], coursesPerSem :: Int }                deriving (Show, Generic)

data Session = Session {
  dayOfWeek :: Int,
  start :: Int,
  duration :: Int
  } deriving (Show, Generic)

data Course = Course {
  name :: String,
  prereqs :: [String],
  sessions :: [Session]
  } deriving (Show, Generic)

data Semester = Semester { 
  sem :: String
  ,courses :: [Course] 
  } deriving (Show, Generic) 

data Program = Program {
  semesters :: [Semester]
  } deriving (Show, Generic)

data Student = Student  {
  studName :: String,
  taken :: [String],
  required :: [String],
  coursesPerSem :: Int } deriving (Show, Generic)

data CourseInstance = CourseInstance {
  sSem :: String,
  sName :: String,
  sPrereqs :: [String],
  sSessions :: [Session]
  } deriving (Show, Generic)

data CourseItem = CourseItem {
  cSemester :: String,
  cCourseName :: String,
  cDay :: Int,
  cStart :: Int,
  cDuration :: Int
} deriving (Show, Generic)

instance FromJSON Student


-- --Returns a list of semesters in reveresed order
-- reverse :: [a] -> [a]
-- reverse (x:xs) = (reverse xs)  ++ [x]
-- reverse x = x
-- reverse [] = []

printStudent :: Student -> IO ()
printStudent student = do
  putStr ("Student Name: " ++ studName student)
  putStr "    Courses Taken: "
  putStr $ intercalate ", " (taken student)
  putStr "    Courses Required: "
  putStr $ intercalate ", " (required student)

printSession :: Session -> IO ()
printSession session = putStrLn ("Day of Week: " ++ show (dayOfWeek session) ++ ", Start: " ++ show (start session) ++ ", Duration: " ++ show (duration session))

printSessionList :: [Session] -> IO ()
printSessionList (x:xs) = do
  printSession x
  printSessionList xs
printSessionList [] = return ()

printCourse :: Course -> IO ()
printCourse course = do
  putStr ("Course Name: " ++ name course)
  putStr "  Prequesites: "
  putStr $ intercalate ", " (prereqs course)
  putStr "  Sessions: "
  printSessionList (sessions course)

printCourseList :: [Course] -> IO ()
printCourseList (x:xs) = do
  printCourse x
  printCourseList xs
printCourseList [] = return ()


printSemester :: Semester -> IO ()
printSemester (Semester sem list) = do
  print sem
  printCourseList list

printSemesterList :: [Semester] -> IO ()
printSemesterList (x:xs) = do
  putStrLn "Semester: "
  printSemester x
  printSemesterList xs
printSemesterList [] = return ()

printProgram :: Program -> IO ()
printProgram (Program list) = printSemesterList list

printProgramList :: [Program] -> IO ()
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
course_list_contains_course :: Course -> [Course] -> Bool
course_list_contains_course  s (l:ls) = sn == ln || course_list_contains_course  s ls
  where
    sn = get_name_from_course s
    ln = get_name_from_course l
course_list_contains_course  s [] = False

-- Returns true if every String in first param list in is second param list (for course use)
course_list_is_course_sublist :: [Course] -> [Course] -> Bool
course_list_is_course_sublist (o:os) (t:ts) = course_list_contains_course o (t:ts) && course_list_is_course_sublist os (t:ts)
course_list_is_course_sublist [] _ = True
course_list_is_course_sublist _ [] = False

--  Checks single against single session. Returns true if two session overlap 
conflict_session_1to1 :: Session -> Session -> Bool
conflict_session_1to1 (Session dow1 st1 dur1) (Session dow2 st2 dur2) = dow1==dow2  && ( st1 <= st2 && st1 + dur1 > st2  ||  st2 <= st1 && st2 + dur2 > st1 )

-- Checks one session against list.
conflict_session_1tolist :: Session -> [Session] -> Bool
conflict_session_1tolist o = foldr ((||) . conflict_session_1to1 o) False

-- Checks list against list. 
conflict_session_listtolist :: [Session] -> [Session] -> Bool
conflict_session_listtolist (o:os) (t:ts) =  conflict_session_1tolist o (t:ts) || conflict_session_listtolist os (t:ts)
conflict_session_listtolist [] (t:ts) = False

-- Returns true if any of sessions conflict
conflict_course_1to1 :: Course -> Course  -> Bool
conflict_course_1to1 (Course _ _ (o:os) ) (Course _ _ (t:ts) ) = conflict_session_listtolist (o:os) (t:ts)
conflict_course_1to1 (Course _ _ [] ) (Course _ _ (t:ts) ) = False
conflict_course_1to1 (Course _ _ (o:os) ) (Course _ _ [] ) = False

-- Returns true if any courses conflict
conflict_course_1tolist :: Course -> [Course]  -> Bool
conflict_course_1tolist o = foldr ((||) . conflict_course_1to1 o) False

-- Given Course, returns name
get_name_from_course :: Course -> String
get_name_from_course (Course n _ _ ) = n

get_names_from_course_list :: [Course] -> [String]
get_names_from_course_list = map get_name_from_course

-- Given Course, returns prereqs
get_prereqs_from_course :: Course -> [String]
get_prereqs_from_course (Course _ p _ ) = p

get_prereqs_from_course_list :: [Course] -> [String]
get_prereqs_from_course_list (c:cs) = remove_duplicates_from_string_list ( get_prereqs_from_course c ++ get_prereqs_from_course_list cs )
get_prereqs_from_course_list [] = []

remove_duplicates_from_string_list :: [String] -> [String]
remove_duplicates_from_string_list (x:xs) | list_contains_string x xs = remove_duplicates_from_string_list xs
                                          | otherwise = x:remove_duplicates_from_string_list xs
remove_duplicates_from_string_list x = x

-- Later: add fall/winter, data from csv for this function 
-- Given courseName and offeredCourses list, returns Course if offered, otherwise returns Course with name = "NA"
get_currently_offered_course :: String -> [Course] -> Course
get_currently_offered_course n [] = Course "NA" [] []
get_currently_offered_course n (c:cs) | name == n = c
                                      | name /= n = get_currently_offered_course n cs
    where name = get_name_from_course c

-- Given a list of semesters, removes all duplicate semesters.
-- use case: for each semester against every other one, check if its courses are subsets of one another, thus they are identical and one should be removed. 
remove_duplicates_from_course_list :: [Semester] -> [Semester]
remove_duplicates_from_course_list (x:xs) | does_semester_occur_in_list x xs = remove_duplicates_from_course_list xs
                                          | otherwise = x:remove_duplicates_from_course_list xs
remove_duplicates_from_course_list x = x

remove_duplicates_from_program_list :: [Program]  -> [Program]
remove_duplicates_from_program_list (p:ps) | does_program_occur_in_list p ps = remove_duplicates_from_program_list ps
                                           | otherwise = p:remove_duplicates_from_program_list ps
remove_duplicates_from_program_list x = x

does_program_occur_in_list :: Program -> [Program] -> Bool
does_program_occur_in_list p (x:xs) = all_course_names_from_program p == all_course_names_from_program x || does_program_occur_in_list p xs
does_program_occur_in_list p x = False

-- Checks if a semester occurs in a list of semesters.
does_semester_occur_in_list :: Semester -> [Semester] -> Bool
does_semester_occur_in_list s (x:xs) = course_list_is_course_sublist sl xl && course_list_is_course_sublist xl sl || does_semester_occur_in_list s xs
    where
      sl = get_courses_from_semester s
      xl = get_courses_from_semester x
does_semester_occur_in_list s x = False

-- Place course at the front of the Course list of a Semester
course_into_semester :: Course -> Semester -> Semester
course_into_semester c (Semester sem cs ) = Semester sem (c:cs)

-- Place semester at the front of the semester list of a program
semester_into_program :: Semester -> Program -> Program
semester_into_program x (Program xs) = Program (x:xs)

-- Given Semester, returns the list of courses within.
get_courses_from_semester :: Semester -> [Course]
get_courses_from_semester (Semester _ l) = l

get_semesters_from_program :: Program -> [Semester]
get_semesters_from_program (Program s) = s

--Returns list of all prereqs (names) of courses in a semester. (removes duplicates)
all_prereq_names_from_semester :: Semester -> [String]
all_prereq_names_from_semester s =  remove_duplicates_from_string_list (get_prereqs_from_course_list c)
  where c = get_courses_from_semester s

--Returns list of all course (names) in a semester. (does not remove duplicates)
all_course_names_from_semester :: Semester -> [String]
all_course_names_from_semester s = get_names_from_course_list c
  where c = get_courses_from_semester s

--Returns list of all courses(names) from list of semesters. (does not remove duplicates)
all_course_names_from_semester_list :: [Semester] -> [String]
all_course_names_from_semester_list = concatMap all_course_names_from_semester

--function that makes list of all courses (names) in a Program. (does not remove duplicates)
all_course_names_from_program :: Program -> [String]
all_course_names_from_program p = all_course_names_from_semester_list s
  where s = get_semesters_from_program p

is_empty :: [a] -> Bool
is_empty (x:xs) = False
is_empty [] = True

takeProgram :: Int -> [Program] -> [Program]
takeProgram 0 _      = []
takeProgram _ []     = []
takeProgram n (x:xs) = x : take (n-1) xs

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

-- semester_recursive :: Program -> Int -> [Course] -> Student -> Program
-- semester_recursive degree coursesPerSem coursesOffered student 
-- solveForColumns :: [Int] -> [Solution]
-- solveForColumns []         = [ [] ]     -- one solution to no columns: an empty list of placements!
-- solveForColumns (col:rest) = [ placement:solRest | solRest <- solveForColumns rest,
--                                                    row     <- range,
--                                                    let placement = (col,row),
--                                                    noAttack placement solRest
--                              ]
-- noAttack :: Placement -> Solution -> Bool     -- determines if a placement is conflict-free with the given list of placements
-- noAttack (x,y) placements = and [ noIndividualAttack p | p <- placements ]
--      where noIndividualAttack (x2,y2) = y /= y2 && abs (y-y2) /= abs (x-x2)     -- not in same row && not in same column

-- course_recursive :: Int -> [Course]
course_recursive :: [Course] -> [Semester] -> [String] -> Int -> Int -> [Course] -> Student -> [Course]
-- course_recursive _ _ _ _ _ _ _ = [Course {name = "COMP5690", prereqs = ["COMP2659"], sessions = [Session {dayOfWeek = 0, start = 25, duration = 1}]}]
-- course_recursive courses semesters coursesRemaining numSemCourses maxCourses coursesOffered student =
-- course_recursive courses semesters [] numSemCourses maxCourses coursesOffered student = [Course {name = "COMP5690", prereqs = ["COMP2659"], sessions = [Session {dayOfWeek = 0, start = 25, duration = 1}]}]
course_recursive courses semesters [] numSemCourses maxCourses coursesOffered student = courses
course_recursive courses semesters (course:remaining) numSemCourses maxCourses coursesOffered student
  | length courses == maxCourses = courses
  | canSchedule = course_recursive (c:courses) semesters remaining numSemCourses maxCourses coursesOffered student
  | otherwise = course_recursive courses semesters remaining numSemCourses maxCourses coursesOffered student
    where
      c = get_currently_offered_course course coursesOffered
      n = get_name_from_course c
      p = get_prereqs_from_course c
      
      -- l = get_courses_from_semester 
      canSchedule = n /= "NA" &&
      --  not ( list_contains_string x (taken student) ) &&  
                                  not ( list_contains_string course (taken student) ) && 
                                  -- not ( list_contains_string course (get_names_from_course_list (get_courses_from_all_semesters semesters)) ) &&        --not taken by student
                                  (list_is_sublist p (get_names_from_course_list (get_courses_from_all_semesters semesters))) &&                  --student has prereqs (relic)   
                                  not (conflict_course_1tolist c courses )                    --no conflict with existing schedule


schedule_recursive :: [Semester] -> [String] -> Int -> Int -> Int -> [Course] -> [Course] -> Student -> [Semester]
schedule_recursive semestersScheduled coursesRemaining currentSem numSemCourses maxCourses coursesFall coursesWinter student
  | currentSem >= numSemCourses = []
  -- | currentSem >= numSemCourses = semestersScheduled
  | coursesRemaining == [] = semestersScheduled
  | otherwise = schedule_recursive ((Semester { sem = semStr, courses = course_recursive [] semestersScheduled updatedCoursesRemaining numSemCourses maxCourses coursesOffered student}):semestersScheduled) updatedCoursesRemaining (currentSem + 1) numSemCourses maxCourses coursesFall coursesWinter student
    where
      coursesOffered = if even currentSem then coursesFall else coursesWinter
      updatedCoursesRemaining = updateRemaining semestersScheduled coursesRemaining
      semStr = if even currentSem then "Fall" else "Winter"


updateRemaining :: [Semester] -> [String] -> [String]
updateRemaining semesters coursesRemaining = coursesRemaining \\ get_names_from_course_list (get_courses_from_all_semesters semesters)

get_courses_from_all_semesters :: [Semester] -> [Course]
get_courses_from_all_semesters [] = []
get_courses_from_all_semesters (s:ss) = get_courses_from_semester s ++ get_courses_from_all_semesters ss

degree_handler :: Int -> Int -> [Course] -> [Course] -> Student -> Program
degree_handler numSemCourses maxCourses coursesFall coursesWinter student
  | numSemCourses <= 0 = Program []
  | maxCourses <= 0 = Program []
  | otherwise = Program { semesters = schedule_recursive [] coursesRemaining 0 numSemCourses maxCourses coursesFall coursesWinter student}
  -- | otherwise = Program { semesters = [Semester {courses = [Course {name = "COMP5690", prereqs = ["COMP2659"], sessions = [Session {dayOfWeek = 0, start = 25, duration = 1}]}]}]}
    where coursesRemaining = required student \\ taken student



-- 0,winter,phil1179,mon/16/mru,wed/16/mru,fri/16/mru,
-- CourseInstance
csvCourseHeader :: CSV.Header
csvCourseHeader =
  Vector.fromList
    [ "Semester"
    , "CourseName"
    , "Day"
    , "Start"
    , "Duration"
    ]

instance CSV.DefaultOrdered CourseItem where
  headerOrder _ =
    CSV.header
      [ "Semester"
      , "CourseName"
      , "Day"
      , "Start"
      , "Duration"
      ]

instance CSV.ToNamedRecord CourseItem where
  toNamedRecord CourseItem{..} =
    CSV.namedRecord
      ["Semester" .= cSemester
    , "CourseName" .= cCourseName
    , "Day" .= cDay
    , "Start" .= cStart
    , "Duration" .= cDuration
      ]

toCourseInstance :: String -> Course -> CourseInstance
toCourseInstance sem (Course name prereqs session) = 
  CourseInstance {
    sSem = sem,
    sName = name, 
    sPrereqs = prereqs, 
    sSessions = session}

toCourseInstances :: String -> [Course] -> [CourseInstance]
toCourseInstances _ [] = []
toCourseInstances sem (x:xs) =  toCourseInstance sem x :(toCourseInstances sem xs) 

semesterToCourseInstances :: Semester -> [CourseInstance]
semesterToCourseInstances (Semester sem cs) = toCourseInstances sem cs 

semestersToCourseInstances :: [Semester] -> [CourseInstance]
semestersToCourseInstances [] = []
semestersToCourseInstances (x:xs) = semesterToCourseInstances x ++ semestersToCourseInstances xs

programCourseInstance :: Program -> [CourseInstance]
programCourseInstance (Program semesters) = semestersToCourseInstances semesters

getDayFromSession :: Session -> Int
getDayFromSession (Session day _ _) = day
getStartFromSession :: Session -> Int
getStartFromSession (Session _ start _) = start
getDurationFromSession :: Session -> Int
getDurationFromSession (Session _ _ dur) = dur

generateCourseItem :: CourseInstance -> CourseItem
generateCourseItem (CourseInstance sem name _ (x:xs)) = CourseItem
  { cSemester = sem -- ++ (i bar)
  , cCourseName = name
  , cDay = getDayFromSession x
  , cStart = getStartFromSession x
  , cDuration = getDurationFromSession x
  }

test_program = Program {
  semesters = [
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "FALL", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]},
        Semester {sem = "WINTER", courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]}
    ]
  }
test_semester = Semester {sem = "FALL",courses = [Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]}]}
test_courses = [
  Course {name = "COMP1633", prereqs = ["COMP1631"], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},
  Course {name = "COMP1631", prereqs = [], sessions = [Session {dayOfWeek = 0, start = 8, duration = 1}]},
  Course {name = "MATH1200", prereqs = [], sessions = [Session {dayOfWeek = 0, start = 10, duration = 1}]}] 


generateCourseItems ::  [CourseInstance] -> Vector CourseItem
generateCourseItems items = Vector.fromList $ generateCourseItemList items
  where generateCourseItemList items = map generateCourseItem items

encodeCourseItems :: Vector CourseItem -> ByteString
encodeCourseItems = encodeDefaultOrderedByName . Foldable.toList

writeCourseItemsToFile :: FilePath -> Vector CourseItem -> IO ()
writeCourseItemsToFile filePath =
  BS.writeFile filePath . encodeCourseItems



writeProgramCSV :: Program -> FilePath -> IO ()
writeProgramCSV program path = do
  let d = programCourseInstance program
  -- print d
  let courseItems = generateCourseItems d
  let items = Vector.reverse courseItems
  writeCourseItemsToFile path items

writeTestProgramsTo:: IO()
writeTestProgramsTo = do
  let d = test_program
  writeProgramCSV d "output.txt"



main :: IO ()
main = do
  args <- getArgs
  -- taken = (take 1 shortestPrograms)

  json <- BSL.readFile (head args)

  case decode json :: Maybe Student of
    -- Just student -> printProgramList (take 1 ( find_shortest_programs 5 (coursesPerSem student) coursesOfferedFall coursesOfferedWinter student ) )
    Just student -> writeProgramCSV ( degree_handler 10 (coursesPerSem student) coursesOfferedFall coursesOfferedWinter student ) "output.txt"
    Nothing -> putStrLn "Failed to parse JSON"
