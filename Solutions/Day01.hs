module Solutions.Day01
( puzzle1
, puzzle2
) where

import Data.List (isPrefixOf, isSuffixOf)

puzzle1 :: [String] -> String
puzzle1 = show . sum . map lineValue

lineValue :: String -> Int
lineValue line = read [firstNum line, lastNum line]

firstNum :: String -> Char
firstNum [] = '0'
firstNum (c:cs)
    | c `elem` ['0'..'9'] = c
    | otherwise           = firstNum cs

lastNum :: String -> Char
lastNum = firstNum . reverse


puzzle2 :: [String] -> String
puzzle2 = show . sum . map lineValueComplex

lineValueComplex :: String -> Int
lineValueComplex line = read [firstNumComplex line, lastNumComplex line]

firstNumComplex :: String -> Char
firstNumComplex [] = '0'
firstNumComplex line@(c:cs)
    | c `elem` ['0'..'9']       = c
    | "zero" `isPrefixOf` line  = '0'
    | "one" `isPrefixOf` line   = '1'
    | "two" `isPrefixOf` line   = '2'
    | "three" `isPrefixOf` line = '3'
    | "four" `isPrefixOf` line  = '4'
    | "five" `isPrefixOf` line  = '5'
    | "six" `isPrefixOf` line   = '6'
    | "seven" `isPrefixOf` line = '7'
    | "eight" `isPrefixOf` line = '8'
    | "nine" `isPrefixOf` line  = '9'
    | otherwise                 = firstNumComplex cs

lastNumComplex :: String -> Char
lastNumComplex [] = '0'
lastNumComplex line
    | l `elem` ['0'..'9']       = l
    | "zero" `isSuffixOf` line  = '0'
    | "one" `isSuffixOf` line   = '1'
    | "two" `isSuffixOf` line   = '2'
    | "three" `isSuffixOf` line = '3'
    | "four" `isSuffixOf` line  = '4'
    | "five" `isSuffixOf` line  = '5'
    | "six" `isSuffixOf` line   = '6'
    | "seven" `isSuffixOf` line = '7'
    | "eight" `isSuffixOf` line = '8'
    | "nine" `isSuffixOf` line  = '9'
    | otherwise                 = lastNumComplex $ init line
    where l = last line

