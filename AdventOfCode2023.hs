import Solutions.Day01
import Solutions.Day02
import Solutions.Day03
import Solutions.Day04
import Solutions.Day05
import Solutions.Day06
import Solutions.Day07
import Solutions.Day08
import Solutions.Day09
import Solutions.Day10
import Solutions.Day11
import Solutions.Day12
import Solutions.Day13
import Solutions.Day14
import Solutions.Day15
import Solutions.Day16

main :: IO ()
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
    day09Input <- readDay "09"
    putStrLn $ "Day 09 Puzzle 1: " ++ Solutions.Day09.puzzle1 day09Input ++ " (Verified: 1581679977)"
    putStrLn $ "Day 09 Puzzle 2: " ++ Solutions.Day09.puzzle2 day09Input ++ " (Verified: 889)"
    day10Input <- readDay "10"
    putStrLn $ "Day 10 Puzzle 1: " ++ Solutions.Day10.puzzle1 day10Input ++ " (Verified: 6812)"
    --putStrLn $ "Day 10 Puzzle 2: " ++ Solutions.Day10.puzzle2 day10Input ++ " (Verified: 527)"
    day11Input <- readDay "11"
    putStrLn $ "Day 11 Puzzle 1: " ++ Solutions.Day11.puzzle1 day11Input ++ " (Verified: 9769724)"
    putStrLn $ "Day 11 Puzzle 2: " ++ Solutions.Day11.puzzle2 day11Input ++ " (Verified: 603020563700)"
    day12Input <- readDay "12"
    putStrLn $ "Day 12 Puzzle 1: " ++ Solutions.Day12.puzzle1 day12Input ++ " (Verified: 7361)"
    --putStrLn $ "Day 12 Puzzle 2: " ++ Solutions.Day12.puzzle2 day12Input ++ " (Verified: X)"
    day13Input <- readDay "13"
    putStrLn $ "Day 13 Puzzle 1: " ++ Solutions.Day13.puzzle1 day13Input ++ " (Verified: 30535)"
    putStrLn $ "Day 13 Puzzle 2: " ++ Solutions.Day13.puzzle2 day13Input ++ " (Verified: 30844)"
    day14Input <- readDay "14"
    putStrLn $ "Day 14 Puzzle 1: " ++ Solutions.Day14.puzzle1 day14Input ++ " (Verified: 113456)"
    --putStrLn $ "Day 14 Puzzle 2: " ++ Solutions.Day14.puzzle2 day14Input ++ " (Verified: X)"
    day15Input <- readDay "15"
    putStrLn $ "Day 15 Puzzle 1: " ++ Solutions.Day15.puzzle1 day15Input ++ " (Verified: 521434)"
    putStrLn $ "Day 15 Puzzle 2: " ++ Solutions.Day15.puzzle2 day15Input ++ " (Verified: 248279)"
    day16Input <- readDay "16"
    putStrLn $ "Day 16 Puzzle 1: " ++ Solutions.Day16.puzzle1 day16Input ++ " (Verified: 7927)"
    putStrLn $ "Day 16 Puzzle 2: " ++ Solutions.Day16.puzzle2 day16Input ++ " (Verified: 8246)"

readDay :: String -> IO ([String])
readDay day = do
    dayInput <- readFile ("day_inputs/day_" ++ day ++ ".txt")
    return (lines dayInput)
