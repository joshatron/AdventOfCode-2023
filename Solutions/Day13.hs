module Solutions.Day13
( puzzle1
, puzzle2
) where

import Data.List.Split (splitOn)

puzzle1 :: [String] -> String
puzzle1 = show . sum . map findReflection . parseTerrains

findReflection :: [[Terrain]] -> Int
findReflection t = (findHorizontalReflection t) + (findVerticalReflection t)

findHorizontalReflection :: [[Terrain]] -> Int
findHorizontalReflection t = sum . map (*100) . filter (isHorizontallyReflective t 0) $ [1..((length t)-1)]

isHorizontallyReflective :: [[Terrain]] -> Int -> Int -> Bool
isHorizontallyReflective t spread center
    | (center-spread-1) < 0 || (center+spread) >= (length t) = True
    | (t!!(center-spread-1)) == (t!!(center+spread))         = isHorizontallyReflective t (spread+1) center
    | otherwise                                              = False

findVerticalReflection :: [[Terrain]] -> Int
findVerticalReflection t = sum . filter (isVerticallyReflective t 0) $ [1..((length (t!!0))-1)]

isVerticallyReflective :: [[Terrain]] -> Int -> Int -> Bool
isVerticallyReflective t spread center
    | (center-spread-1) < 0 || (center+spread) >= (length (t!!0))  = True
    | (map (!!(center-spread-1)) t) == (map (!!(center+spread)) t) = isVerticallyReflective t (spread+1) center
    | otherwise                                                    = False

parseTerrains :: [String] -> [[[Terrain]]]
parseTerrains = map parseTerrain . splitOn [""]

parseTerrain :: [String] -> [[Terrain]]
parseTerrain = map parseRow

parseRow :: String -> [Terrain]
parseRow = map parseLocation

parseLocation :: Char -> Terrain
parseLocation '.' = Ash
parseLocation '#' = Rock
parseLocation _   = Unknown

data Terrain = Ash | Rock | Unknown deriving (Show, Eq)

puzzle2 :: [String] -> String
puzzle2 = show . sum . map findReflectionSmudge . parseTerrains

findReflectionSmudge :: [[Terrain]] -> Int
findReflectionSmudge t = (findHorizontalReflectionSmudge t) + (findVerticalReflectionSmudge t)

findHorizontalReflectionSmudge :: [[Terrain]] -> Int
findHorizontalReflectionSmudge t = sum . map (*100) . filter (\r -> (getHorizontalDiff t 0 r) == 1) $ [1..((length t)-1)]

findVerticalReflectionSmudge :: [[Terrain]] -> Int
findVerticalReflectionSmudge t = sum . filter (\c -> (getVerticalDiff t 0 c) == 1) $ [1..((length (t!!0))-1)]

getHorizontalDiff :: [[Terrain]] -> Int -> Int -> Int
getHorizontalDiff t spread center
    | (center-spread-1) < 0 || (center+spread) >= (length t) = 0
    | otherwise                                              = (getRowDiff (t!!(center-spread-1)) (t!!(center+spread))) + (getHorizontalDiff t (spread+1) center)

getVerticalDiff :: [[Terrain]] -> Int -> Int -> Int
getVerticalDiff t spread center
    | (center-spread-1) < 0 || (center+spread) >= (length (t!!0)) = 0
    | otherwise                                                   = (getRowDiff (map (!!(center-spread-1)) t) (map (!!(center+spread)) t)) + (getVerticalDiff t (spread+1) center)

getRowDiff :: [Terrain] -> [Terrain] -> Int
getRowDiff [] [] = 0
getRowDiff (t1:t1s) (t2:t2s)
    | t1 == t2  = getRowDiff t1s t2s
    | otherwise = 1 + getRowDiff t1s t2s

