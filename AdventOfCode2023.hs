import System.IO 
import Solutions.Day01
import Solutions.Day02
import Solutions.Day03
import Solutions.Day04
import Solutions.Day05
import Solutions.Day06
import Solutions.Day07
import Solutions.Day08


main = do 
    day01Input <- readDay "01"
    putStrLn $ "Day 01 Puzzle 1: " ++ Solutions.Day01.puzzle1 day01Input ++ " (Verified: 55386)"
    putStrLn $ "Day 01 Puzzle 2: " ++ Solutions.Day01.puzzle2 day01Input ++ " (Verified: 54824)"
    day02Input <- readDay "02"
    putStrLn $ "Day 02 Puzzle 1: " ++ Solutions.Day02.puzzle1 day02Input ++ " (Verified: 2204)"
    putStrLn $ "Day 02 Puzzle 2: " ++ Solutions.Day02.puzzle2 day02Input ++ " (Verified: 71036)"
    day03Input <- readDay "03"
    putStrLn $ "Day 03 Puzzle 1: " ++ Solutions.Day03.puzzle1 day03Input ++ " (Verified: 538046)"
    putStrLn $ "Day 03 Puzzle 2: " ++ Solutions.Day03.puzzle2 day03Input ++ " (Verified: 81709807)"
    day04Input <- readDay "04"
    putStrLn $ "Day 04 Puzzle 1: " ++ Solutions.Day04.puzzle1 day04Input ++ " (Verified: 25651)"
    putStrLn $ "Day 04 Puzzle 2: " ++ Solutions.Day04.puzzle2 day04Input ++ " (Verified: 19499881)"
    day05Input <- readDay "05"
    putStrLn $ "Day 05 Puzzle 1: " ++ Solutions.Day05.puzzle1 day05Input ++ " (Verified: 806029445)"
    putStrLn $ "Day 05 Puzzle 2: " ++ Solutions.Day05.puzzle2 day05Input ++ " (Verified: 59370572)"
    day06Input <- readDay "06"
    putStrLn $ "Day 06 Puzzle 1: " ++ Solutions.Day06.puzzle1 day06Input ++ " (Verified: 2269432)"
    putStrLn $ "Day 06 Puzzle 2: " ++ Solutions.Day06.puzzle2 day06Input ++ " (Verified: 35865985)"
    day07Input <- readDay "07"
    putStrLn $ "Day 07 Puzzle 1: " ++ Solutions.Day07.puzzle1 day07Input ++ " (Verified: 246912307)"
    putStrLn $ "Day 07 Puzzle 2: " ++ Solutions.Day07.puzzle2 day07Input ++ " (Verified: 246894760)"
    day08Input <- readDay "08"
    putStrLn $ "Day 08 Puzzle 1: " ++ Solutions.Day08.puzzle1 day08Input ++ " (Verified: 17873)"
    putStrLn $ "Day 08 Puzzle 2: " ++ Solutions.Day08.puzzle2 day08Input ++ " (Verified: 15746133679061)"

readDay :: String -> IO ([String])
readDay day = do
    dayInput <- readFile ("day_inputs/day_" ++ day ++ ".txt")
    return (lines dayInput)
