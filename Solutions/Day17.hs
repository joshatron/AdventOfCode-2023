module Solutions.Day17
( puzzle1
, puzzle2
) where

import Data.Char(digitToInt)
import Data.List(find)
import Data.Maybe(fromJust)
import qualified Data.Heap as Heap
import qualified Data.Set as Set
import qualified Data.Map as Map

type OpenList = Heap.MinPrioHeap Int SearchNode
type CloseList = Set.Set (Point, Int)

puzzle1 :: [String] -> String
puzzle1 = show . currentHeatLoss . findShortestPath . parseHeatMap

findShortestPath :: HeatMap -> SearchNode
findShortestPath heatMap = aStarSearch heatMap (Point ((width heatMap)-1) ((height heatMap)-1)) (Heap.singleton (0, SearchNode (Point 0 0) 0 East 0 [])) Set.empty
--findShortestPath heatMap = aStarSearch heatMap (Point 40 40) (Heap.singleton (0, SearchNode (Point 0 0) 0 East 0 [])) Set.empty

aStarSearch :: HeatMap -> Point -> OpenList -> CloseList -> SearchNode
aStarSearch heatMap destination openList closeList
    | anyAtDestination children destination = getAtDestination children destination
    | otherwise                             = aStarSearch heatMap destination (addChildrenToOpenList destination remainder closeList children) (Set.insert ((location q), (stepsForward q)) closeList)
    where ((f, q), remainder) = fromJust (Heap.view openList)
          children = determineChildren heatMap q

addChildrenToOpenList :: Point -> OpenList -> CloseList -> [SearchNode] -> OpenList
addChildrenToOpenList destination openList closeList nodes = foldr (\n acc -> addChildToOpenList destination acc closeList n) openList nodes

addChildToOpenList :: Point -> OpenList -> CloseList -> SearchNode -> OpenList
addChildToOpenList destination openList closeList node@(SearchNode p g d s _)
    | noneBetterInClose && noneBetterInOpen = Heap.insert (f, node) openList
    | otherwise                             = openList
    where f = g + (manhattanDistance destination p)
          noneBetterInClose = not ((p, s) `Set.member` closeList)
          noneBetterInOpen = Heap.null $ Heap.filter (\(openf,(SearchNode op _ _ os _)) -> op == p && os <= s && openf < f) openList

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = (abs (x1-x2)) + (abs (y1-y2))

getAtDestination :: [SearchNode] -> Point -> SearchNode
getAtDestination nodes destination = fromJust $ find (\(SearchNode p _ _ _ _) -> p == destination) nodes

anyAtDestination :: [SearchNode] -> Point -> Bool
anyAtDestination nodes destination = destination `elem` (map location nodes)

determineChildren :: HeatMap -> SearchNode -> [SearchNode]
determineChildren heatMap (SearchNode p g dir forward history)
    | canGoFront && canGoLeft && canGoRight = [(SearchNode toLeft (g+(getInMap heatMap toLeft)) leftDir 1 (p:history)), (SearchNode toRight (g+(getInMap heatMap toRight)) rightDir 1 (p:history)), (SearchNode inFront (g+(getInMap heatMap inFront)) dir (forward+1) (p:history))]
    | canGoFront && canGoLeft               = [(SearchNode toLeft (g+(getInMap heatMap toLeft)) leftDir 1 (p:history)), (SearchNode inFront (g+(getInMap heatMap inFront)) dir (forward+1) (p:history))]
    | canGoFront && canGoRight              = [(SearchNode toRight (g+(getInMap heatMap toRight)) rightDir 1 (p:history)), (SearchNode inFront (g+(getInMap heatMap inFront)) dir (forward+1) (p:history))]
    | canGoLeft && canGoRight               = [(SearchNode toLeft (g+(getInMap heatMap toLeft)) leftDir 1 (p:history)), (SearchNode toRight (g+(getInMap heatMap toRight)) rightDir 1 (p:history))]
    | canGoLeft                             = [(SearchNode toLeft (g+(getInMap heatMap toLeft)) leftDir 1 (p:history))]
    | canGoRight                            = [(SearchNode toRight (g+(getInMap heatMap toRight)) rightDir 1 (p:history))]
    | otherwise                             = []
    where inFront    = moveForwardOne p dir
          toLeft     = moveLeftOne p dir
          leftDir    = turnLeft dir
          toRight    = moveRightOne p dir
          rightDir   = turnRight dir
          canGoFront = forward < 3 && inMap heatMap inFront
          canGoLeft  = inMap heatMap toLeft
          canGoRight = inMap heatMap toRight

moveForwardOne :: Point -> Direction -> Point
moveForwardOne (Point x y) North = Point x (y-1)
moveForwardOne (Point x y) South = Point x (y+1)
moveForwardOne (Point x y) East  = Point (x+1) y
moveForwardOne (Point x y) West  = Point (x-1) y

moveLeftOne :: Point -> Direction -> Point
moveLeftOne (Point x y) North = Point (x-1) y
moveLeftOne (Point x y) South = Point (x+1) y
moveLeftOne (Point x y) East  = Point x (y-1)
moveLeftOne (Point x y) West  = Point x (y+1)

moveRightOne :: Point -> Direction -> Point
moveRightOne (Point x y) North = Point (x+1) y
moveRightOne (Point x y) South = Point (x-1) y
moveRightOne (Point x y) East  = Point x (y+1)
moveRightOne (Point x y) West  = Point x (y-1)

turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight East  = South
turnRight West  = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft East  = North
turnLeft West  = South

getInMap :: HeatMap -> Point -> Int
getInMap (HeatMap _ _ c) p = fromJust (Map.lookup p c)

inMap :: HeatMap -> Point -> Bool
inMap (HeatMap w h _) (Point x y) = x >= 0 && y >= 0 && x < w && y < h

data HeatMap = HeatMap { width :: Int
                       , height :: Int
                       , costs :: Map.Map Point Int
                       } deriving (Show)

data SearchNode = SearchNode { location :: Point
                             , currentHeatLoss :: Int
                             , direction :: Direction
                             , stepsForward :: Int
                             , previousSteps :: [Point]
                             } deriving (Show)

data Point = Point { xLoc :: Int
                   , yLoc :: Int
                   } deriving (Show, Eq, Ord)

data Direction = North | South | East | West deriving (Show, Eq, Ord)

parseHeatMap :: [String] -> HeatMap
parseHeatMap lines = HeatMap (length (head lines)) (length lines) (Map.fromList . concat . map parseRow . zip [0..] $ lines)

parseRow :: (Int, String) -> [(Point, Int)]
parseRow (y, line) = map (\(x,c) -> (Point x y, digitToInt c)) . zip [0..] $ line

puzzle2 :: [String] -> String
puzzle2 _ = "N/A"
