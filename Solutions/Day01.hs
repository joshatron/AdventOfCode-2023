module Solutions.Day01
( puzzle1
, puzzle2
) where

import Data.List (isPrefixOf, isSuffixOf)

puzzle1 :: [String] -> String
puzzle1 lines = show (addLines lines)

addLines :: [String] -> Int
addLines [] = 0
addLines lines = lineValue (head lines) + (addLines (tail lines))

lineValue :: String -> Int
lineValue line = read ([firstNum line] ++ [lastNum line])

firstNum :: String -> Char
firstNum [] = '0'
firstNum line
    | head line `elem` ['0'..'9'] = head line
    | otherwise                   = firstNum (tail line)

lastNum :: String -> Char
lastNum line = firstNum (reverse line)

puzzle2 :: [String] -> String
puzzle2 lines = show (addLinesComplex lines)

addLinesComplex :: [String] -> Int
addLinesComplex [] = 0
addLinesComplex lines = lineValueComplex (head lines) + (addLinesComplex (tail lines))

lineValueComplex :: String -> Int
lineValueComplex line = read ([firstNumComplex line] ++ [lastNumComplex line])

firstNumComplex :: String -> Char
firstNumComplex [] = '0'
firstNumComplex line
    | head line `elem` ['0'..'9'] = head line
    | isPrefixOf "zero" line      = '0'
    | isPrefixOf "one" line       = '1'
    | isPrefixOf "two" line       = '2'
    | isPrefixOf "three" line     = '3'
    | isPrefixOf "four" line      = '4'
    | isPrefixOf "five" line      = '5'
    | isPrefixOf "six" line       = '6'
    | isPrefixOf "seven" line     = '7'
    | isPrefixOf "eight" line     = '8'
    | isPrefixOf "nine" line      = '9'
    | otherwise                   = firstNumComplex (tail line)

lastNumComplex :: String -> Char
lastNumComplex [] = '0'
lastNumComplex line
    | last line `elem` ['0'..'9'] = last line
    | isSuffixOf "zero" line      = '0'
    | isSuffixOf "one" line       = '1'
    | isSuffixOf "two" line       = '2'
    | isSuffixOf "three" line     = '3'
    | isSuffixOf "four" line      = '4'
    | isSuffixOf "five" line      = '5'
    | isSuffixOf "six" line       = '6'
    | isSuffixOf "seven" line     = '7'
    | isSuffixOf "eight" line     = '8'
    | isSuffixOf "nine" line      = '9'
    | otherwise                   = lastNumComplex (init line)

