module Solutions.Day08
( puzzle1
, puzzle2
) where

import qualified Data.Map as Map

puzzle1 :: [String] -> String
puzzle1 lines = show $ stepsToZZZ (parseNodes lines) (parseInstructions lines) "AAA"

stepsToZZZ :: Map.Map String Node -> [Direction] -> String -> Int
stepsToZZZ _ _ "ZZZ" = 0
stepsToZZZ nodes (d:ds) current
    | d == L = 1 + (stepsToZZZ nodes ds (maybe "ZZZ" l (Map.lookup current nodes)))
    | d == R = 1 + (stepsToZZZ nodes ds (maybe "ZZZ" r (Map.lookup current nodes)))

data Direction = L | R deriving (Show, Eq)

data Node = Node { name :: String
                 , l    :: String
                 , r    :: String
                 } deriving (Show)

parseNodes :: [String] -> Map.Map String Node
parseNodes = Map.fromList . map parseNode . tail . tail

parseNode :: String -> (String, Node)
parseNode line = (n, Node n l r)
    where n = head . words $ line
          l = init . tail . head . tail . tail . words $ line
          r = init . last . words $ line

parseInstructions :: [String] -> [Direction]
parseInstructions = cycle . map parseInstruction . head

parseInstruction :: Char -> Direction
parseInstruction 'L' = L
parseInstruction 'R' = R

puzzle2 :: [String] -> String
puzzle2 lines = show . lcmAll . map (stepsToAnyZ nodes instructions) $ startNodes
    where nodes = parseNodes lines
          instructions = parseInstructions lines
          startNodes = getStartNodes (Map.keys nodes)

lcmAll :: [Int] -> Int
lcmAll = foldl1 lcm

stepsToAnyZ :: Map.Map String Node -> [Direction] -> String -> Int
stepsToAnyZ nodes (d:ds) current
    | last current == 'Z' = 0
    | d == L              = 1 + (stepsToAnyZ nodes ds (maybe "ZZZ" l (Map.lookup current nodes)))
    | d == R              = 1 + (stepsToAnyZ nodes ds (maybe "ZZZ" r (Map.lookup current nodes)))

getStartNodes :: [String] -> [String]
getStartNodes = filter (\n -> (last n) == 'A')
