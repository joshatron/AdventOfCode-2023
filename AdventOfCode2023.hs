import System.IO 
import Solutions.Day01
import Solutions.Day02


main = do 
    day01Input <- readDay "01"
    putStrLn $ "Day 01 Puzzle 1: " ++ Solutions.Day01.puzzle1 day01Input ++ " (Verified: 55386)"
    putStrLn $ "Day 01 Puzzle 2: " ++ Solutions.Day01.puzzle2 day01Input ++ " (Verified: 54824)"
    day02Input <- readDay "02"
    putStrLn $ "Day 02 Puzzle 1: " ++ Solutions.Day02.puzzle1 day02Input ++ " (Verified: 2204)"
    putStrLn $ "Day 02 Puzzle 2: " ++ Solutions.Day02.puzzle2 day02Input ++ " (Verified: 71036)"

readDay :: String -> IO ([String])
readDay day = do
    dayInput <- readFile ("day_inputs/day_" ++ day ++ ".txt")
    return (lines dayInput)
