import System.IO 
import Solutions.Day01


main = do 
    day01Input <- readFile "day_inputs/day_01.txt" 
    putStrLn $ "Day 01 Puzzle 1: " ++ Solutions.Day01.puzzle1 (lines day01Input)
    putStrLn $ "Day 01 PUzzle 2: " ++ Solutions.Day01.puzzle2 (lines day01Input)
