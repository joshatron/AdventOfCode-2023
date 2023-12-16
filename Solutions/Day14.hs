module Solutions.Day14
( puzzle1
, puzzle2
) where

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Set as Set

puzzle1 :: [String] -> String
puzzle1 lines = show . calculateLoad $ moveRocksInDirection (parsePlatform lines) North

calculateLoad :: Platform -> Int
calculateLoad platform@(Platform _ _ r _) = sum $ map (calculateLoadRock platform) (Set.toList r)

calculateLoadRock :: Platform -> Point -> Int
calculateLoadRock (Platform _ h _ _) (Point x y) = h - y

moveRocksInDirection :: Platform -> Direction -> Platform
moveRocksInDirection platform@(Platform w h r s) dir = Platform w h (foldr (\p acc -> Set.insert (moveRockInDirection (Platform w h acc s) dir p) acc) Set.empty (sortByDirection platform dir)) s

moveRockInDirection :: Platform -> Direction -> Point -> Point
moveRockInDirection platform dir point
    | isPointValid platform toMove = moveRockInDirection platform dir toMove
    | otherwise                    = point
    where toMove = movePointInDirection dir point

isPointValid :: Platform -> Point -> Bool
isPointValid (Platform w h r s) point@(Point x y)
    | x < 0                = False
    | y < 0                = False
    | x >= w               = False
    | y >= h               = False
    | point `Set.member` r = False
    | point `Set.member` s = False
    | otherwise            = True

movePointInDirection :: Direction -> Point -> Point
movePointInDirection North (Point x y) = Point x (y-1)
movePointInDirection South (Point x y) = Point x (y+1)
movePointInDirection East (Point x y)  = Point (x+1) y
movePointInDirection West (Point x y)  = Point (x-1) y

sortByDirection :: Platform -> Direction -> [Point]
sortByDirection (Platform _ _ points _) North = sortBy (flip compare `on` yLoc) (Set.toList points)
sortByDirection (Platform _ _ points _) South = sortBy (compare `on` yLoc) (Set.toList points)
sortByDirection (Platform _ _ points _) East  = sortBy (compare `on` xLoc) (Set.toList points)
sortByDirection (Platform _ _ points _) West  = sortBy (flip compare `on` xLoc) (Set.toList points)

parsePlatform :: [String] -> Platform
parsePlatform lines = Platform (length (head lines)) (length lines) (parseChar lines 'O') (parseChar lines '#')

parseChar :: [String] -> Char -> Set.Set Point
parseChar lines char = Set.fromList . concat . map (parseCharRow char) $ zip lines [0..]

parseCharRow :: Char -> (String, Int) -> [Point]
parseCharRow char (line, y) = map (\x -> Point x y) $ map snd $ filter (\(c,_) -> char == c) $ zip line [0..]

data Platform = Platform { width :: Int
                         , height :: Int
                         , round :: Set.Set Point
                         , square :: Set.Set Point
                         } deriving (Show, Eq)

data Point = Point { xLoc :: Int
                   , yLoc :: Int
                   } deriving (Show, Eq, Ord)

data Direction = North | South | East | West deriving (Show, Eq)

puzzle2 :: [String] -> String
puzzle2 lines = show . calculateLoad $ (runInfiniteCycles platform)!!(numberOfIterations platform)
    where platform = parsePlatform lines

numberOfIterations :: Platform -> Int
numberOfIterations platform = 100 + ((1000000000-100) `mod` (findCycle platform))

findCycle :: Platform -> Int
findCycle platform = fst . fromJust $ find (\(n,p) -> p == first) $ zip [1..] (runInfiniteCycles (runOneCycle first))
    where first = (runInfiniteCycles platform)!!100

runInfiniteCycles :: Platform -> [Platform]
runInfiniteCycles platform = iterate runOneCycle platform

runOneCycle :: Platform -> Platform
runOneCycle platform = moveRocksInDirection (moveRocksInDirection (moveRocksInDirection (moveRocksInDirection platform North) West) South) East

toString :: Platform -> String
toString platform = ['\n'] ++ (concat (map (toStringRow platform) [0..((height platform)-1)]))

toStringRow :: Platform -> Int -> String
toStringRow platform y =  (map (toStringLoc platform y) [0..((width platform)-1)]) ++ ['\n']

toStringLoc :: Platform -> Int -> Int -> Char
toStringLoc (Platform w h r s) y x
    | p `elem` r = 'O'
    | p `elem` s = '#'
    | otherwise  = '.'
    where p = Point x y
