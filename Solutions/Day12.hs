module Solutions.Day12
( puzzle1
, puzzle2
) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

puzzle1 :: [String] -> String
puzzle1 = show . sum . map countValidReplacements . parseRows

countValidReplacements :: ([Spring],[Int]) -> Int
countValidReplacements state = countValidReplacementsTracking state False

countValidReplacementsTracking :: ([Spring],[Int]) -> Bool -> Int
countValidReplacementsTracking ([],[]) _ = 1
countValidReplacementsTracking ([],0:[]) _ = 1
countValidReplacementsTracking ((s:ss),[]) _ 
    | s == Working || s == Unknown = countValidReplacementsTracking (ss,[]) False
    | otherwise                    = 0
countValidReplacementsTracking ([],d:ds) _ = 0
countValidReplacementsTracking ((s:ss),groups@(d:ds)) inDamaged
    | s == Damaged && groups == [] = 0
    | s == Damaged && not inDamaged = countValidReplacementsTracking (ss,(d-1):ds) True
    | s == Damaged && inDamaged && d == 0 = 0
    | s == Damaged && inDamaged && d > 0  = countValidReplacementsTracking (ss,(d-1):ds) True
    | s == Working && not inDamaged = countValidReplacementsTracking (ss,groups) False
    | s == Working && inDamaged && groups == [] = countValidReplacementsTracking (ss,groups) False
    | s == Working && inDamaged && d > 0 = 0
    | s == Working && inDamaged && d == 0 = countValidReplacementsTracking (ss,ds) False
    | s == Unknown = (countValidReplacementsTracking (Working:ss,groups) inDamaged) + (countValidReplacementsTracking (Damaged:ss,groups) inDamaged) 

parseRows :: [String] -> [([Spring],[Int])]
parseRows = map parseRow

parseRow :: String -> ([Spring],[Int])
parseRow line = (parseSpringStates (parts!!0), parseDamagedGroups (parts!!1))
    where parts = words line

parseSpringStates :: String -> [Spring]
parseSpringStates = map parseSpringState

parseSpringState :: Char -> Spring
parseSpringState c
    | c == '#' = Damaged
    | c == '.' = Working
    | c == '?' = Unknown

parseDamagedGroups :: String -> [Int]
parseDamagedGroups = map read . splitOn ","

data Spring = Damaged | Working | Unknown deriving (Show, Eq, Ord)

puzzle2 :: [String] -> String
--puzzle2 = show . sum . map countValidReplacements . unfold . parseRows
puzzle2 = show . sum . map countValidReplacements . unfold . parseRows

unfold :: [([Spring],[Int])] -> [([Spring],[Int])]
unfold = map unfoldOne

unfoldOne :: ([Spring],[Int]) -> ([Spring],[Int])
unfoldOne (springs,groups) = (unfoldSprings springs, unfoldGroups groups)

unfoldSprings :: [Spring] -> [Spring]
unfoldSprings springs = init . concat . take 5 . repeat $ springs ++ [Unknown]

unfoldGroups :: [Int] -> [Int]
unfoldGroups = concat . take 5 . repeat
