module Solutions.Day02
( puzzle1
, puzzle2
) where

import Data.List.Split (chunksOf)

puzzle1 :: [String] -> String
puzzle1 = show . sum . map getGameId . filter isPossibleGame

isPossibleGame :: String -> Bool
isPossibleGame game = red <= 12 && green <= 13 && blue <= 14
    where (red, green, blue) = getMaxColors game

getMaxColors :: String -> (Int, Int, Int)
getMaxColors game = (getMaxColor game "red", getMaxColor game "green", getMaxColor game "blue")

getMaxColor :: String -> String -> Int
getMaxColor game color = maximum . map (getPairValue color) . chunksOf 2 . words . filter (/=',') . filter (/=';') $ game

getPairValue :: String -> [String] -> Int
getPairValue desiredColor (num:color:[]) 
    | color == desiredColor = read num
    | otherwise             = 0

getGameId :: String -> Int
getGameId = read . head . tail . words . filter (/=':')


puzzle2 :: [String] -> String
puzzle2 = show . sum . map getGamePower

getGamePower :: String -> Int
getGamePower game = (getMaxColor game "red") * (getMaxColor game "green") * (getMaxColor game "blue")
