module Solutions.Day03
( puzzle1
, puzzle2
) where

puzzle1 :: [String] -> String
puzzle1 = show . sum . getPartNumbers

getPartNumbers :: [String] -> [Int]
getPartNumbers = findNumbersUnderInfluence . identifySymbolInfluence

identifySymbolInfluence :: [String] -> [[(Char, Bool)]]
identifySymbolInfluence lines = map (\row -> map (\coordinate -> coordinateToCharAndInfluence lines coordinate) row) (generateGridOfCoordinates (length (lines!!0)) (length lines))

generateGridOfCoordinates :: Int -> Int -> [[(Int, Int)]]
generateGridOfCoordinates x y = reverse (generateGridOfCoordinatesUpsideDown x (y-1))

generateGridOfCoordinatesUpsideDown :: Int -> Int -> [[(Int, Int)]]
generateGridOfCoordinatesUpsideDown x y
    | y < 0     = []
    | otherwise = (reverse (generateRowOfCoordinatesBackward (x-1) y)):(generateGridOfCoordinatesUpsideDown x (y-1))

generateRowOfCoordinatesBackward :: Int -> Int -> [(Int, Int)]
generateRowOfCoordinatesBackward x y
    | x < 0     = []
    | otherwise = (x, y):(generateRowOfCoordinatesBackward (x-1) y)

coordinateToCharAndInfluence:: [String] -> (Int,Int) -> (Char, Bool)
coordinateToCharAndInfluence lines (x,y) = (((lines!!y)!!x), isSymbol lines (x-1) (y-1) || isSymbol lines x (y-1) || isSymbol lines (x+1) (y-1) || 
                                                             isSymbol lines (x-1) y || isSymbol lines x y || isSymbol lines (x+1) y || 
                                                             isSymbol lines (x-1) (y+1) || isSymbol lines x (y+1) || isSymbol lines (x+1) (y+1))

isSymbol :: [String] -> Int -> Int -> Bool
isSymbol lines x y
    | x < 0                         = False
    | y < 0                         = False
    | x >= length (lines!!0)        = False
    | y >= length lines             = False
    | lines!!y!!x `elem` ['0'..'9'] = False
    | lines!!y!!x == '.'            = False
    | otherwise                     = True

findNumbersUnderInfluence :: [[(Char, Bool)]] -> [Int]
findNumbersUnderInfluence [] = []
findNumbersUnderInfluence (l:ls) = (findNumbersUnderInfluenceRow l "" False) ++ findNumbersUnderInfluence ls

findNumbersUnderInfluenceRow :: [(Char, Bool)] -> String -> Bool -> [Int]
findNumbersUnderInfluenceRow [] acc isPart
    | length acc == 0 = []
    | isPart          = (read acc):[]
    | otherwise       = []
findNumbersUnderInfluenceRow ((c,p):cs) acc isPart
    | c `elem` ['0'..'9']      = findNumbersUnderInfluenceRow cs (acc ++ [c]) (isPart || p)
    | length acc > 0 && isPart = (read acc):findNumbersUnderInfluenceRow cs "" False
    | otherwise                = findNumbersUnderInfluenceRow cs "" False


puzzle2 :: [String] -> String
puzzle2 = show . sum . getGearRatios

getGearRatios :: [String] -> [Int]
getGearRatios lines = map (getGearRatio lines) (findGears lines)

getGearRatio :: [String] -> (Int, Int) -> Int
getGearRatio lines coord
    | length surroundingParts == 2 = (surroundingParts!!0) * (surroundingParts!!1)
    | otherwise                    = 0
    where surroundingParts = getSurroundingParts lines coord

getSurroundingParts :: [String] -> (Int, Int) -> [Int]
getSurroundingParts lines coord = map (expandPartNumber lines) (filterOutNonParts lines (getStartingPoints lines coord))

expandPartNumber :: [String] -> (Int, Int) -> Int
expandPartNumber lines coord = read (readPartNumber lines (findLeftOfPartNumber lines coord))

readPartNumber :: [String] -> (Int, Int) -> String
readPartNumber lines (x, y)
    | c `elem` ['0'..'9'] = c:readPartNumber lines (x+1, y)
    | otherwise           = []
    where c = safeGetAt lines (x, y)

findLeftOfPartNumber :: [String] -> (Int, Int) -> (Int, Int)
findLeftOfPartNumber lines (x, y)
    | (safeGetAt lines (x-1, y)) `elem` ['0'..'9'] = findLeftOfPartNumber lines (x-1, y)
    | otherwise                                  = (x, y)

filterOutNonParts :: [String] -> [(Int, Int)] -> [(Int, Int)]
filterOutNonParts lines points = filter (\p -> (safeGetAt lines p) `elem` ['0'..'9']) points

getStartingPoints :: [String] -> (Int, Int) -> [(Int, Int)]
getStartingPoints lines (x, y)
    | multTop && multBottom = [(x-1, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x+1, y+1)]
    | multTop               = [(x-1, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x, y+1)]
    | multBottom            = [(x, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x+1, y+1)]
    | otherwise             = [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]
    where multTop = safeGetAt lines (x, y-1) == '.'
          multBottom = safeGetAt lines (x, y+1) == '.'

safeGetAt :: [String] -> (Int, Int) -> Char
safeGetAt lines (x, y)
    | x < 0                  = '.'
    | y < 0                  = '.'
    | x >= length (lines!!0) = '.'
    | y >= length lines      = '.'
    | otherwise              = lines!!y!!x

findGears :: [String] -> [(Int, Int)]
findGears lines = filterGears lines (generateGridOfCoordinates (length (lines!!0)) (length lines))

filterGears :: [String] -> [[(Int, Int)]] -> [(Int, Int)]
filterGears lines coords = concat (map (filterGearsRow lines) coords)

filterGearsRow :: [String] -> [(Int, Int)] -> [(Int, Int)]
filterGearsRow lines coords = filter (isGear lines) coords

isGear :: [String] -> (Int, Int) -> Bool
isGear lines (x, y) = lines!!y!!x == '*'

