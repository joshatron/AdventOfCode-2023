module Solutions.Day09
( puzzle1
, puzzle2
) where

puzzle1 :: [String] -> String
puzzle1 = show . sum . map nextValue . map parseLine

nextValue :: [Int] -> Int
nextValue values
    | identical values = head values
    | otherwise        = (last values) + (nextValue (differentiate values))

differentiate :: [Int] -> [Int]
differentiate (i:[]) = []
differentiate (i:is) = ((head is) - i):(differentiate is)

identical :: [Int] -> Bool
identical (i:[]) = True
identical (i:is)
    | i == head is = identical is
    | otherwise    = False

parseLine :: String -> [Int]
parseLine = map read . words

puzzle2 :: [String] -> String
puzzle2 = show . sum . map previousValue . map parseLine

previousValue :: [Int] -> Int
previousValue values
    | identical values = head values
    | otherwise        = (head values) - (previousValue (differentiate values))
