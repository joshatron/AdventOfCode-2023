module Solutions.Day18
( puzzle1
, puzzle2
) where

import qualified Data.Set as Set

puzzle1 :: [String] -> String
puzzle1 = show . countPointsInside . getAllPoints . parseLineSegments

countPointsInside :: Set.Set Point -> Int
countPointsInside points = sum . map (countPointsInsideLine points bounding) $ [minY..maxY]
    where bounding@((Point _ minY), (Point _ maxY)) = getBoundingBox points

countPointsInsideLine :: Set.Set Point -> (Point, Point) -> Int -> Int
countPointsInsideLine points ((Point minX _), (Point maxX _)) y = total
    where (total, _, _) = foldl (checkPoint points y) (0, False, Middle) [minX..maxX]


checkPoint :: Set.Set Point -> Int -> (Int, Bool, Side) -> Int -> (Int, Bool, Side)
checkPoint points y (total, inside, side) x
    | not onLine && inside                     = (total+1, inside, Middle)
    | not onLine && not inside                 = (total, inside, Middle)
    | onLine && lineAbove && lineBelow         = (total+1, not inside, Middle)
    | onLine && not lineAbove && not lineBelow = (total+1, inside, side)
    | onLine && side == Middle && lineAbove    = (total+1, inside, Below)
    | onLine && side == Middle && lineBelow    = (total+1, inside, Above)
    | onLine && side == Above && lineAbove     = (total+1, not inside, Middle)
    | onLine && side == Above && lineBelow     = (total+1, inside, Middle)
    | onLine && side == Below && lineAbove     = (total+1, inside, Middle)
    | onLine && side == Below && lineBelow     = (total+1, not inside, Middle)
    where onLine = (Point x y) `Set.member` points
          lineAbove = (Point x (y-1)) `Set.member` points
          lineBelow = (Point x (y+1)) `Set.member` points

getBoundingBox :: Set.Set Point -> (Point, Point)
getBoundingBox points = (Point minX minY, Point maxX maxY)
    where minX = minimum . map xLoc . Set.toList $ points
          minY = minimum . map yLoc . Set.toList $ points
          maxX = maximum . map xLoc . Set.toList $ points
          maxY = maximum . map yLoc . Set.toList $ points

getAllPoints :: [LineSegment] -> Set.Set Point
getAllPoints = Set.fromList . concat . map points

parseLineSegments :: [String] -> [LineSegment]
parseLineSegments = snd . foldl addLineSegment ((Point 0 0), [])

addLineSegment :: (Point, [LineSegment]) -> String -> (Point, [LineSegment])
addLineSegment (p, segments) line = (last (points newSegment), newSegment:segments)
    where newSegment = parseLineSegment line p

parseLineSegment :: String -> Point -> LineSegment
parseLineSegment line start = LineSegment (followLineSegment start dir dist) color
    where parts = words line
          dir = read (head parts)
          dist = read (parts!!1)
          color = init (tail (parts!!2))

followLineSegment :: Point -> Direction -> Int -> [Point]
followLineSegment start dir 0 = []
followLineSegment start dir dist = next:(followLineSegment next dir (dist-1))
    where next = moveOne start dir

moveOne :: Point -> Direction -> Point
moveOne (Point x y) U = Point x (y-1)
moveOne (Point x y) D = Point x (y+1)
moveOne (Point x y) L = Point (x-1) y
moveOne (Point x y) R = Point (x+1) y

data Side = Above | Below | Middle deriving (Show, Eq)

data Direction = U | D | L | R deriving (Show, Read, Eq)

data LineSegment = LineSegment { points :: [Point]
                               , colorCode :: String
                               } deriving (Show)

data Point = Point { xLoc :: Int
                   , yLoc :: Int
                   } deriving (Show, Eq, Ord)

puzzle2 :: [String] -> String
puzzle2 _ = "N/A"
