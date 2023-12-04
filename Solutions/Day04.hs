module Solutions.Day04
( puzzle1
, puzzle2
) where

puzzle1 :: [String] -> String
puzzle1 = show . sum . map calculatePointsForCard

calculatePointsForCard :: String -> Int
calculatePointsForCard card = calculateScore (getWinningNumbers card) (getYourNumbers card)

calculateScore :: [Int] -> [Int] -> Int
calculateScore winning = multScore . length . filter (\n -> n `elem` winning)

multScore :: Int -> Int
multScore matching
    | matching == 0 = 0
    | otherwise     = 2 ^ (matching - 1)

getWinningNumbers :: String -> [Int]
getWinningNumbers = map read . takeWhile (/="|") . tail . tail . words

getYourNumbers :: String -> [Int]
getYourNumbers = reverse . map read . takeWhile (/="|") . reverse . words

puzzle2 :: [String] -> String
puzzle2 = show . countUpCopies . map getNumberMatching

countUpCopies :: [Int] -> Int
countUpCopies cards = countUpCopiesTrackingDuplicates cards []

countUpCopiesTrackingDuplicates :: [Int] -> [(Int, Int)] -> Int
countUpCopiesTrackingDuplicates [] _ = 0
countUpCopiesTrackingDuplicates (c:cs) duplicating
    | c == 0    = numCards + (countUpCopiesTrackingDuplicates cs (countDownDuplicates duplicating))
    | otherwise = numCards + (countUpCopiesTrackingDuplicates cs ((numCards, c):(countDownDuplicates duplicating)))
    where numCards = duplicates duplicating

duplicates :: [(Int, Int)] -> Int
duplicates [] = 1
duplicates ((num,_):ds) = num + (duplicates ds)

countDownDuplicates :: [(Int, Int)] -> [(Int, Int)]
countDownDuplicates [] = []
countDownDuplicates ((numCards,matches):ds)
    | matches == 1 = countDownDuplicates ds
    | otherwise    = (numCards, matches - 1):(countDownDuplicates ds)

getNumberMatching :: String -> Int
getNumberMatching line = length (checkForMatches (getWinningNumbers line) (getYourNumbers line))

checkForMatches :: [Int] -> [Int] -> [Int]
checkForMatches winning = filter (\n -> n `elem` winning)
