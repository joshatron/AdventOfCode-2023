module Solutions.Day14
( puzzle1
, puzzle2
) where

import Data.List
import Data.Function
import qualified Data.Set as Set

puzzle1 :: [String] -> String
puzzle1 lines = show $ calculateLoad lines $ fst $ moveRocksInDirection lines (parseRoundRocks lines, parseSquareRocks lines) North

calculateLoad :: [String] -> [Point] -> Int
calculateLoad lines points = sum $ map (calculateLoadRock lines) points

calculateLoadRock :: [String] -> Point -> Int
calculateLoadRock lines point = (length lines) - (y point)

moveRocksInDirection :: [String] -> ([Point],[Point]) -> Direction -> ([Point],[Point])
moveRocksInDirection lines (round,square) dir = (foldl (\acc p -> (moveRockInDirection lines (acc,square) dir p):acc) [] (sortByDirection round dir), square)

moveRockInDirection :: [String] -> ([Point],[Point]) -> Direction -> Point -> Point
moveRockInDirection lines obstructions dir point
    | isPointOutOfBounds lines toMove       = point
    | isPointObstructed obstructions toMove = point
    | otherwise                             = moveRockInDirection lines obstructions dir toMove
    where toMove = movePointInDirection dir point

sortByDirection :: [Point] -> Direction -> [Point]
sortByDirection points North = sortBy (compare `on` y) points
sortByDirection points South = sortBy (flip compare `on` y) points
sortByDirection points East  = sortBy (flip compare `on` x) points
sortByDirection points West  = sortBy (compare `on` x) points

parseSquareRocks :: [String] -> [Point]
parseSquareRocks lines = parseChar lines '#'

parseRoundRocks :: [String] -> [Point]
parseRoundRocks lines = parseChar lines 'O'

parseChar :: [String] -> Char -> [Point]
parseChar lines char = concat $ map (parseCharRow char) $ zip lines [0..]

parseCharRow :: Char -> (String, Int) -> [Point]
parseCharRow char (line, y) = map (\x -> Point x y) $ map snd $ filter (\(c,_) -> char == c) $ zip line [0..]

isPointOutOfBounds :: [String] -> Point -> Bool
isPointOutOfBounds lines point
    | (x point) < 0                      = True
    | (y point) < 0                      = True
    | (x point) >= (length (head lines)) = True
    | (y point) >= (length lines)        = True
    | otherwise                          = False

isPointObstructed :: ([Point],[Point]) -> Point -> Bool
isPointObstructed (round,square) point = (point `elem` round) || (point `elem` square)

movePointInDirection :: Direction -> Point -> Point
movePointInDirection North point = Point (x point) ((y point)-1)
movePointInDirection South point = Point (x point) ((y point)+1)
movePointInDirection East point  = Point ((x point)+1) (y point)
movePointInDirection West point  = Point ((x point)-1) (y point)

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq, Ord)

data Direction = North | South | East | West deriving (Show, Eq)

puzzle2 :: [String] -> String
--puzzle2 lines = (toString $ (parseRoundRocks lines, parseSquareRocks lines)) ++ (toString $ moveRocksInDirection lines (parseRoundRocks lines, parseSquareRocks lines) North) ++ (toString $ moveRocksInDirection lines (moveRocksInDirection lines (parseRoundRocks lines, parseSquareRocks lines) North) West) ++ "\n" ++ (show $ sortByDirection (fst $ (moveRocksInDirection lines (parseRoundRocks lines, parseSquareRocks lines) North)) West) ++ "\n" ++ (show (fst $ moveRocksInDirection lines (moveRocksInDirection lines (parseRoundRocks lines, parseSquareRocks lines) North) West)) ++ "\n" ++ (toString $ moveRocksInDirection lines (moveRocksInDirection lines (moveRocksInDirection lines (parseRoundRocks lines, parseSquareRocks lines) North) West) South) ++ (toString $ runCycle lines (parseRoundRocks lines, parseSquareRocks lines))
--puzzle2 lines = toString $ runCycle lines $ runCycle lines $ runCycle lines (parseRoundRocks lines, parseSquareRocks lines)
puzzle2 lines = show $ findCycle lines

findCycle :: [String] -> Int
findCycle lines = findMatch lines thousand thousand
    where initial     = (parseRoundRocks lines, parseSquareRocks lines)
          thousand = iterate (runCycle lines) initial !! 99999

findMatch :: [String] -> ([Point],[Point]) -> ([Point],[Point]) -> Int
findMatch lines initial current
    | Set.fromList (fst next) == Set.fromList (fst initial) = 0
    | otherwise                                             = 1 + (findMatch lines initial next)
    where next = runCycle lines current

runCycle :: [String] -> ([Point],[Point]) -> ([Point],[Point])
runCycle lines rocks = moveRocksInDirection lines (moveRocksInDirection lines (moveRocksInDirection lines (moveRocksInDirection lines rocks North) West) South) East

toString :: ([Point],[Point]) -> String
toString (round,square) = ['\n'] ++ (concat (map (toStringRow (round,square)) [0..(height)]))
    where height = maximum ((map y round) ++ (map y square))

toStringRow :: ([Point],[Point]) -> Int -> String
toStringRow (round,square) y =  (map (toStringLoc (round,square) y) [0..(width)]) ++ ['\n']
    where width = maximum ((map x round) ++ (map x square))

toStringLoc :: ([Point],[Point]) -> Int -> Int -> Char
toStringLoc (round,square) y x
    | p `elem` round  = 'O'
    | p `elem` square = '#'
    | otherwise       = '.'
    where p = Point x y
