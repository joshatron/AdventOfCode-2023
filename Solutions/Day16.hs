module Solutions.Day16
( puzzle1
, puzzle2
) where

import qualified Data.Set as Set

puzzle1 :: [String] -> String
puzzle1 lines = show . length $ countWhatLightBeamTouches (parseContraption lines) (LightBeam (Point 0 0) East)

countWhatLightBeamTouches :: Contraption -> LightBeam -> [Point]
countWhatLightBeamTouches contraption initialBeam  = Set.toList . Set.map beamLoc $ recursivelyFollowLightBeam contraption initialBeam Set.empty

recursivelyFollowLightBeam :: Contraption -> LightBeam -> Set.Set LightBeam -> Set.Set LightBeam
recursivelyFollowLightBeam contraption beam@(LightBeam point dir) visited
    | not (pointInContraption contraption point) = visited
    | beam `Set.member` visited                  = visited
    | beamWillBeSplit tile beam                  = recursivelyFollowLightBeam contraption (moveBeam tile beam True) (recursivelyFollowLightBeam contraption (moveBeam tile beam False) (Set.insert beam visited))
    | otherwise                                  = recursivelyFollowLightBeam contraption (moveBeam tile beam True) (Set.insert beam visited)
    where tile = getTileAt contraption point

moveBeam :: Tile -> LightBeam -> Bool -> LightBeam
moveBeam (Tile _ tType) (LightBeam point dir) splitFirst
    | tType == ForwardDiagonal && dir == North                      = LightBeam (movePointInDir point East) East
    | tType == ForwardDiagonal && dir == South                      = LightBeam (movePointInDir point West) West
    | tType == ForwardDiagonal && dir == East                       = LightBeam (movePointInDir point North) North
    | tType == ForwardDiagonal && dir == West                       = LightBeam (movePointInDir point South) South
    | tType == BackwardDiagonal && dir == North                     = LightBeam (movePointInDir point West) West
    | tType == BackwardDiagonal && dir == South                     = LightBeam (movePointInDir point East) East
    | tType == BackwardDiagonal && dir == East                      = LightBeam (movePointInDir point South) South
    | tType == BackwardDiagonal && dir == West                      = LightBeam (movePointInDir point North) North
    | tType == HorizontalSplitter && dir == North && splitFirst     = LightBeam (movePointInDir point West) West
    | tType == HorizontalSplitter && dir == North && not splitFirst = LightBeam (movePointInDir point East) East
    | tType == HorizontalSplitter && dir == South && splitFirst     = LightBeam (movePointInDir point West) West
    | tType == HorizontalSplitter && dir == South && not splitFirst = LightBeam (movePointInDir point East) East
    | tType == VerticalSplitter && dir == East && splitFirst        = LightBeam (movePointInDir point North) North
    | tType == VerticalSplitter && dir == East && not splitFirst    = LightBeam (movePointInDir point South) South
    | tType == VerticalSplitter && dir == West && splitFirst        = LightBeam (movePointInDir point North) North
    | tType == VerticalSplitter && dir == West && not splitFirst    = LightBeam (movePointInDir point South) South
    | otherwise                                                     = LightBeam (movePointInDir point dir) dir

movePointInDir :: Point -> Direction -> Point
movePointInDir (Point x y) North = Point x (y-1)
movePointInDir (Point x y) South = Point x (y+1)
movePointInDir (Point x y) East  = Point (x+1) y
movePointInDir (Point x y) West  = Point (x-1) y

beamWillBeSplit :: Tile -> LightBeam -> Bool
beamWillBeSplit (Tile _ tType) (LightBeam point dir)
    | dir == North && tType == HorizontalSplitter = True
    | dir == South && tType == HorizontalSplitter = True
    | dir == East && tType == VerticalSplitter    = True
    | dir == West && tType == VerticalSplitter    = True
    | otherwise                                   = False

getTileAt :: Contraption -> Point -> Tile
getTileAt (Contraption _ _ tiles) point
    | found == [] = Tile point Empty
    | otherwise   = head found
    where found = filter (\(Tile p _) -> p == point) tiles

pointInContraption :: Contraption -> Point -> Bool
pointInContraption (Contraption width height _) (Point x y)
    | x < 0       = False
    | y < 0       = False
    | x >= width  = False
    | y >= height = False
    | otherwise   = True

parseContraption :: [String] -> Contraption
parseContraption lines = Contraption (length (head lines)) (length lines) (parseTiles lines)

parseTiles :: [String] -> [Tile]
parseTiles = concat . map parseRow . zip [0..]

parseRow :: (Int, String) -> [Tile]
parseRow (y, row) = filter (\t -> (tileType t) /= Empty) . map (parseTile y) . zip [0..] $ row

parseTile :: Int -> (Int, Char) -> Tile
parseTile y (x, '/')  = Tile (Point x y) ForwardDiagonal
parseTile y (x, '\\') = Tile (Point x y) BackwardDiagonal
parseTile y (x, '|')  = Tile (Point x y) VerticalSplitter
parseTile y (x, '-')  = Tile (Point x y) HorizontalSplitter
parseTile y (x, _)    = Tile (Point x y) Empty

data LightBeam = LightBeam { beamLoc :: Point
                           , direction :: Direction
                           } deriving (Show, Eq, Ord)

data Direction = North | South | East | West deriving (Show, Eq, Ord)

data Contraption = Contraption { width :: Int
                               , height :: Int
                               , tiles :: [Tile]
                               } deriving (Show)

data Tile = Tile { loc :: Point
                 , tileType :: TileType
                 } deriving (Show, Eq)

data TileType = Empty | ForwardDiagonal | BackwardDiagonal | HorizontalSplitter | VerticalSplitter deriving (Show, Eq)

data Point = Point { xLoc :: Int
                   , yLoc :: Int
                   } deriving (Show, Eq, Ord)

puzzle2 :: [String] -> String
puzzle2 lines = show . maximum . map length . map (countWhatLightBeamTouches contraption) $ getAllStartingBeams contraption
    where contraption = parseContraption lines

getAllStartingBeams :: Contraption -> [LightBeam]
getAllStartingBeams (Contraption w h _) = (map (\y -> LightBeam (Point 0 y) East) [0..(h-1)]) ++ (map (\y -> LightBeam (Point (w-1) y) West) [0..(h-1)]) ++ (map (\x -> LightBeam (Point x 0) South) [0..(w-1)]) ++ (map (\x -> LightBeam (Point x (h-1)) North) [0..(w-1)])
