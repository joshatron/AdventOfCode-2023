module Solutions.Day06
( puzzle1
, puzzle2
) where

puzzle1 :: [String] -> String
puzzle1 = show . foldl1 (*) . map waysToBeatRecord . parseTimeDistance

waysToBeatRecord :: (Int, Int) -> Int
waysToBeatRecord (time, dist) = length (filter (\h -> beatsRecord time h dist) [1..time])

beatsRecord :: Int -> Int -> Int -> Bool
beatsRecord time hold dist = (time - hold) * hold > dist

parseTimeDistance :: [String] -> [(Int, Int)]
parseTimeDistance lines = zip (map read . tail . words . head $ lines) (map read . tail . words . head . tail $ lines)

puzzle2 :: [String] -> String
puzzle2 = show . waysToBeatRecordFast . parseTimeDistanceAlt

waysToBeatRecordFast :: (Int, Int) -> Int
waysToBeatRecordFast (time, dist) = (findBound True (time `div` 2) (time `div` 4) time dist) - (findBound False (time `div` 2) (time `div` 4) time dist) + 1

findBound :: Bool -> Int -> Int -> Int -> Int -> Int
findBound top test jump time dist
    | wins && top && not (beatsRecord time (test+1) dist)     = test
    | wins && not top && not (beatsRecord time (test-1) dist) = test
    | wins && top                                             = findBound top (test + jump) (max 1 (jump `div` 2)) time dist
    | not wins && top                                         = findBound top (test - jump) (max 1 (jump `div` 2)) time dist
    | wins && not top                                         = findBound top (test - jump) (max 1 (jump `div` 2)) time dist
    | not wins && not top                                     = findBound top (test + jump) (max 1 (jump `div` 2)) time dist
    where wins = beatsRecord time test dist

parseTimeDistanceAlt :: [String] -> (Int, Int)
parseTimeDistanceAlt lines = ((read . concat . tail . words . head $ lines), (read . concat . tail . words . head . tail $ lines))
