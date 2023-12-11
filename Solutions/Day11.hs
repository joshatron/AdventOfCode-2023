module Solutions.Day11
( puzzle1
, puzzle2
) where

puzzle1 :: [String] -> String
puzzle1 lines = show $ getGalaxyPairLengths $ expandGalaxy (getOriginalGalaxyPoints lines) (getEmptyColumns lines) (getEmptyRows lines)

getGalaxyPairLengths :: [Point] -> Int
getGalaxyPairLengths (p:[]) = 0
getGalaxyPairLengths (p:ps) = (sum (map (getLengthBetweenPoints p) ps)) + (getGalaxyPairLengths ps)

getLengthBetweenPoints :: Point -> Point -> Int
getLengthBetweenPoints p1 p2 = (abs ((x p1) - (x p2))) + (abs ((y p1) - (y p2)))

expandGalaxy :: [Point] -> [Int] -> [Int] -> [Point]
expandGalaxy points columns rows = map (\p -> Point ((x p) + (length (filter (<(x p)) columns))) ((y p) + (length (filter (<(y p)) rows)))) points

getOriginalGalaxyPoints :: [String] -> [Point]
getOriginalGalaxyPoints lines = concat $ map getOriginalGalaxyPointsRow $ zip [0..] lines

getOriginalGalaxyPointsRow :: (Int, String) -> [Point]
getOriginalGalaxyPointsRow (y, line) = map (\(x,_) -> Point x y)$ filter (\(x,c) -> c == '#') $ zip [0..] line

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show)

getEmptyRows :: [String] -> [Int]
getEmptyRows lines = map fst $ filter (\(c,l) -> isRowEmpty l) $ zip [0..] lines

isRowEmpty :: String -> Bool
isRowEmpty = not . any (=='#')

getEmptyColumns :: [String] -> [Int]
getEmptyColumns lines = map fst $ filter (\(c,e) -> e) $ map (\c -> (c,(isColumnEmpty lines c))) [0..((length (lines!!0))-1)]

isColumnEmpty :: [String] -> Int -> Bool
isColumnEmpty lines index = not $ any (=='#') $ map (!!index) lines

puzzle2 :: [String] -> String
puzzle2 lines = show $ getGalaxyPairLengths $ expandGalaxyAMillion (getOriginalGalaxyPoints lines) (getEmptyColumns lines) (getEmptyRows lines)

expandGalaxyAMillion :: [Point] -> [Int] -> [Int] -> [Point]
expandGalaxyAMillion points columns rows = map (\p -> Point ((x p) + ((length (filter (<(x p)) columns)) * 999999)) ((y p) + ((length (filter (<(y p)) rows)) * 999999))) points
