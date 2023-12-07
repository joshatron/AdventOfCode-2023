module Solutions.Day07
( puzzle1
, puzzle2
) where

import Data.List (sort, sortBy, group)

puzzle1 :: [String] -> String
puzzle1 = show . addUpHands . map snd . sortBy compareHands . parseHands

addUpHands :: [Int] -> Int
addUpHands hands = sum $ map (\(b,r) -> b * r) (zip [1..] hands)

compareHands :: (String, Int) -> (String, Int) -> Ordering
compareHands (a,_) (b,_)
    | ha /= hb  = compare ha hb
    | otherwise = compareCards a b
    where ha = handRank a
          hb = handRank b

handRank :: String -> Int
handRank hand
    | groups == [5]         = 7
    | groups == [4,1]       = 6
    | groups == [3,2]       = 5
    | groups == [3,1,1]     = 4
    | groups == [2,2,1]     = 3
    | groups == [2,1,1,1]   = 2
    | groups == [1,1,1,1,1] = 1
    where groups = cardGroups hand

compareCards :: String -> String -> Ordering
compareCards [] [] = EQ
compareCards (a:as) (b:bs)
    | ra /= rb  = compare ra rb
    | otherwise = compareCards as bs
    where ra = cardRank a
          rb = cardRank b

cardRank :: Char -> Int
cardRank c
    | c == 'A' = 13
    | c == 'K' = 12
    | c == 'Q' = 11
    | c == 'J' = 10
    | c == 'T' = 9
    | c == '9' = 8
    | c == '8' = 7
    | c == '7' = 6
    | c == '6' = 5
    | c == '5' = 4
    | c == '4' = 3
    | c == '3' = 2
    | c == '2' = 1

cardGroups :: String -> [Int]
cardGroups = reverse . sort . map length . group . sort

parseHands :: [String] -> [(String, Int)]
parseHands = map parseHand

parseHand :: String -> (String, Int)
parseHand line = (parts!!0, read (parts!!1))
    where parts = words line

puzzle2 :: [String] -> String
puzzle2 = show . addUpHands . map snd . sortBy compareHandsJokers . parseHands

compareHandsJokers :: (String, Int) -> (String, Int) -> Ordering
compareHandsJokers (a,_) (b,_)
    | ha /= hb  = compare ha hb
    | otherwise = compareCardsJokers a b
    where ha = handRankJokers a
          hb = handRankJokers b

handRankJokers :: String -> Int
handRankJokers hand
    | groups == [5]         = 7
    | groups == [4,1]       = 6
    | groups == [3,2]       = 5
    | groups == [3,1,1]     = 4
    | groups == [2,2,1]     = 3
    | groups == [2,1,1,1]   = 2
    | groups == [1,1,1,1,1] = 1
    where groups = cardGroupsJokers hand

compareCardsJokers :: String -> String -> Ordering
compareCardsJokers [] [] = EQ
compareCardsJokers (a:as) (b:bs)
    | ra /= rb  = compare ra rb
    | otherwise = compareCardsJokers as bs
    where ra = cardRankJokers a
          rb = cardRankJokers b

cardRankJokers :: Char -> Int
cardRankJokers c
    | c == 'A' = 13
    | c == 'K' = 12
    | c == 'Q' = 11
    | c == 'T' = 10
    | c == '9' = 9
    | c == '8' = 8
    | c == '7' = 7
    | c == '6' = 6
    | c == '5' = 5
    | c == '4' = 4
    | c == '3' = 3
    | c == '2' = 2
    | c == 'J' = 1

cardGroupsJokers :: String -> [Int]
cardGroupsJokers hand = addJokersBackIn numJokers . reverse . sort . map length . group . sort $ woutJokers
    where woutJokers = filter (/='J') hand
          numJokers = length (filter (=='J') hand)

addJokersBackIn :: Int -> [Int] -> [Int]
addJokersBackIn jokers [] = jokers:[]
addJokersBackIn jokers (c:cs) = (c+jokers):cs
