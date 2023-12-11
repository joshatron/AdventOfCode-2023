module Solutions.Day10
( puzzle1
, puzzle2
) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

puzzle1 :: [String] -> String
puzzle1 lines = show $ countLoopSize (parsePipes lines) `div` 2

countLoopSize :: Pipes -> Int
countLoopSize p = moveThroughLoopCounting p start (findNextAfterStart p)
    where start = findStart p

moveThroughLoopCounting :: Pipes -> Point -> (Direction, Point) -> Int
moveThroughLoopCounting p start current@(dir, currentPoint)
    | currentPoint == start = 1
    | otherwise             = 1 + moveThroughLoopCounting p start (findNext p current)

findNext :: Pipes -> (Direction, Point) -> (Direction, Point)
findNext p (dir, point)
    | shape == NorthAndSouth && dir == South = (South, movePoint South point)
    | shape == NorthAndSouth && dir == North = (North, movePoint North point)
    | shape == NorthAndEast && dir == South  = (East, movePoint East point)
    | shape == NorthAndEast && dir == West   = (North, movePoint North point)
    | shape == NorthAndWest && dir == South  = (West, movePoint West point)
    | shape == NorthAndWest && dir == East   = (North, movePoint North point)
    | shape == EastAndWest && dir == West    = (West, movePoint West point)
    | shape == EastAndWest && dir == East    = (East, movePoint East point)
    | shape == SouthAndEast && dir == North  = (East, movePoint East point)
    | shape == SouthAndEast && dir == West   = (South, movePoint South point)
    | shape == SouthAndWest && dir == North  = (West, movePoint West point)
    | shape == SouthAndWest && dir == East   = (South, movePoint South point)
    where shape = getPipeShapeAt p point

findNextAfterStart :: Pipes -> (Direction, Point)
findNextAfterStart p
    | north == NorthAndSouth || north == SouthAndEast || north == SouthAndWest = (North, movePoint North start)
    | south == NorthAndSouth || south == NorthAndEast || south == NorthAndWest = (South, movePoint South start)
    | east == EastAndWest || east == NorthAndWest || east == SouthAndWest      = (East, movePoint East start)
    | west == EastAndWest || west == NorthAndEast || west == SouthAndEast      = (West, movePoint West start)
    where start = findStart p
          sx    = x start
          sy    = y start
          north = getPipeShapeAt p (movePoint North start)
          south = getPipeShapeAt p (movePoint South start)
          east  = getPipeShapeAt p (movePoint East start)
          west  = getPipeShapeAt p (movePoint West start)

findStart :: Pipes -> Point
findStart = head . Map.keys . Map.filter (==Start) . pipes

getPipeShapeAt :: Pipes -> Point -> PipeShape
getPipeShapeAt p point = fromMaybe Empty (Map.lookup point (pipes p))

parsePipes :: [String] -> Pipes
parsePipes lines = Pipes (length (head lines)) (length lines) (parsePipeShapes lines)

parsePipeShapes :: [String] -> Map.Map Point PipeShape
parsePipeShapes = Map.fromList . concat . map parsePipeShapeLine . zip [0..]

parsePipeShapeLine :: (Int, String) -> [(Point, PipeShape)]
parsePipeShapeLine (y, line) = map (parsePipeShapePoint y) (zip [0..] line)

movePoint :: Direction -> Point -> Point
movePoint North point = Point (x point) (y point - 1) 
movePoint South point = Point (x point) (y point + 1) 
movePoint East point  = Point (x point + 1) (y point) 
movePoint West point  = Point (x point - 1) (y point) 

parsePipeShapePoint :: Int -> (Int, Char) -> (Point, PipeShape)
parsePipeShapePoint y (x, c)
    | c == '|'  = (Point x y, NorthAndSouth)
    | c == 'J'  = (Point x y, NorthAndWest)
    | c == 'L'  = (Point x y, NorthAndEast)
    | c == '-'  = (Point x y, EastAndWest)
    | c == 'F'  = (Point x y, SouthAndEast)
    | c == '7'  = (Point x y, SouthAndWest)
    | c == 'S'  = (Point x y, Start)
    | otherwise = (Point x y, Empty)

data Pipes = Pipes { width  :: Int
                   , height :: Int
                   , pipes  :: Map.Map Point PipeShape
                   } deriving (Show)

data PipeShape = NorthAndSouth | NorthAndEast | NorthAndWest | EastAndWest | SouthAndEast | SouthAndWest | Start | Empty deriving (Show, Eq)

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Ord, Eq)

data Direction = North | South | East | West deriving (Show, Eq)


puzzle2 :: [String] -> String
--puzzle2 lines = show $ getPointsInLoop (parsePipes lines)
--puzzle2 lines = show $ length (getPointsNotInLoop (parsePipes lines))
--puzzle2 lines = show $ intersectionsWithLoop (parsePipes lines) (getPointsInLoop (parsePipes lines)) (Point 3 3) Middle
puzzle2 lines = show $ length (getPointsInsideLoop (parsePipes lines))

getPointsInsideLoop :: Pipes -> [Point]
getPointsInsideLoop p = filter (\point -> isPointInsideLoop replacedStart inLoop point) (getPointsNotInLoop p)
    where inLoop = getPointsInLoop p
          replacedStart = replaceStartWithNormalPoint p

isPointInsideLoop :: Pipes -> [Point] -> Point -> Bool
isPointInsideLoop p inLoop point = not (even (intersectionsWithLoop p inLoop point Middle))

intersectionsWithLoop :: Pipes -> [Point] -> Point -> PassingOn -> Int
intersectionsWithLoop p inLoop point passingOn
    | y point >= height p                     = 0
    | not isIn                                = 0 + (intersectionsWithLoop p inLoop (movePoint South point) Middle)
    | shape == EastAndWest                    = 1 + (intersectionsWithLoop p inLoop (movePoint South point) Middle)
    | shape == NorthAndSouth                  = 0 + (intersectionsWithLoop p inLoop (movePoint South point) passingOn)
    | shape == SouthAndEast                   = 0 + (intersectionsWithLoop p inLoop (movePoint South point) L)
    | shape == SouthAndWest                   = 0 + (intersectionsWithLoop p inLoop (movePoint South point) R)
    | shape == NorthAndWest && passingOn == L = 1 + (intersectionsWithLoop p inLoop (movePoint South point) Middle)
    | shape == NorthAndWest && passingOn == R = 0 + (intersectionsWithLoop p inLoop (movePoint South point) Middle)
    | shape == NorthAndEast && passingOn == L = 0 + (intersectionsWithLoop p inLoop (movePoint South point) Middle)
    | shape == NorthAndEast && passingOn == R = 1 + (intersectionsWithLoop p inLoop (movePoint South point) Middle)
    where isIn = point `elem` inLoop
          shape = getPipeShapeAt p point

getPointsNotInLoop :: Pipes -> [Point]
getPointsNotInLoop p = filter (\point -> not (point `elem` inLoop)) . Map.keys . pipes $ p
    where inLoop = getPointsInLoop p

replaceStartWithNormalPoint :: Pipes -> Pipes
replaceStartWithNormalPoint p
    | northGood && southGood = Pipes (width p) (height p) (Map.insert start NorthAndSouth (pipes p))
    | northGood && eastGood  = Pipes (width p) (height p) (Map.insert start NorthAndEast (pipes p))
    | northGood && westGood  = Pipes (width p) (height p) (Map.insert start NorthAndWest (pipes p))
    | eastGood && westGood   = Pipes (width p) (height p) (Map.insert start EastAndWest (pipes p))
    | southGood && eastGood  = Pipes (width p) (height p) (Map.insert start SouthAndEast (pipes p))
    | southGood && westGood  = Pipes (width p) (height p) (Map.insert start SouthAndWest (pipes p))
    where start = findStart p
          sx    = x start
          sy    = y start
          north = getPipeShapeAt p (movePoint North start)
          south = getPipeShapeAt p (movePoint South start)
          east  = getPipeShapeAt p (movePoint East start)
          west  = getPipeShapeAt p (movePoint West start)
          northGood = north == NorthAndSouth || north == SouthAndEast || north == SouthAndWest
          southGood = south == NorthAndSouth || south == NorthAndEast || south == NorthAndWest
          eastGood  = east == EastAndWest || east == NorthAndWest || east == SouthAndWest
          westGood  = west == EastAndWest || west == NorthAndEast || west == SouthAndEast

getPointsInLoop :: Pipes -> [Point]
getPointsInLoop p = getPointsInLoopInner p start (findNextAfterStart p)
    where start = findStart p

getPointsInLoopInner :: Pipes -> Point -> (Direction, Point) -> [Point]
getPointsInLoopInner p start current@(dir, currentPoint)
    | currentPoint == start = currentPoint:[]
    | otherwise             = currentPoint:(getPointsInLoopInner p start (findNext p current))


data PassingOn = Middle | L | R deriving (Show, Eq)

