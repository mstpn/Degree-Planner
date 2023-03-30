solve :: Problem -> Solution
solve - = [ sol | ... ]

--If we are writing to the screen, then we don't want to try and embed this IO into the list comprehension. Use the main :: pattern

main :: String -> IO() 
-- main path = 
    --somehow open up the file here and read it in

--would be really nice if we had a function called readProblem
--readProblem will open up the file and read it in

readProblem :: String -> IO Problem
--helper print function
printSolution :: Solution -> IO()
--these IO problems are not that hard to implement... very procedural

main path = 
    --note we can't call solve on (readProblem path), since readProblem returns an IO Problem..
    -- use the sequence operator 
    -- (>>=) :: IO a -> (a -> IO b) -> IO b
    readProblem path >>= 
        \p -> solve p --returns list of solutions.. but can't stop here since we main returns type IO... so do IO

-- lets actually rerwite with do syntax
main path =
    do p <- readProblem path
        case solve p of
            [] -> putStr "couldn't do ..."
            (sol _) -> printSolution sol


----------------------------------------------------
emptySched :: Schedule
add ...

solve :: [Course] -> [Schedule]
solve [] = [emptySchedule]
solve (c:cs) = [add c slot schedlist    | schedRest <- solve cs
                                        | slot <- [..]
                                        check c slot schedRest]

----------------------------------

--MoreExemplars file on d2l has more functions to review, some of which include IO
