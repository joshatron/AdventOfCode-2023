module Solutions.Day05
( puzzle1
, puzzle2
) where

import Data.List.Split (chunksOf)

puzzle1 :: [String] -> String
puzzle1 lines = show . minimum $ runSeedsThroughMaps (parseSeeds lines) (parseMaps lines)

runSeedsThroughMaps :: [Int] -> [[(Int, Int, Int)]] -> [Int]
runSeedsThroughMaps seeds maps = map (runSeedThroughMaps maps) seeds

runSeedThroughMaps :: [[(Int, Int, Int)]] -> Int -> Int
runSeedThroughMaps maps seed = foldl (\s m -> runSeedThroughMap s m) seed maps

runSeedThroughMap :: Int -> [(Int, Int, Int)] -> Int
runSeedThroughMap seed [] = seed
runSeedThroughMap seed ((begin,end,adder):ms)
    | seed >= begin && seed <= end = seed + adder
    | otherwise                    = runSeedThroughMap seed ms

parseSeeds :: [String] -> [Int]
parseSeeds = map read . tail . words . head

parseMaps :: [String] -> [[(Int, Int, Int)]]
parseMaps = groupMaps [] . map parseLine

groupMaps :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [[(Int, Int, Int)]]
groupMaps acc [] = acc:[]
groupMaps acc (r:rs)
    | r == (0,0,0) && acc /= [] = acc:(groupMaps [] rs)
    | r == (0,0,0)              = groupMaps [] rs
    |otherwise                  = groupMaps (r:acc) rs

parseLine :: String -> (Int, Int, Int)
parseLine line 
    | line /= "" && (head line) `elem` ['0'..'9'] = parseLineUnsafe line
    | otherwise                                   = (0,0,0)

parseLineUnsafe :: String -> (Int, Int, Int)
parseLineUnsafe line = (nums!!1, nums!!1 + nums!!2 - 1, nums!!0 - nums!!1)
    where nums = map read (words line)


puzzle2 :: [String] -> String
puzzle2 lines = show $ determineMinimum (runSeedsThroughMapsAlt (parseSeedsAlt lines) (parseMaps lines))
--puzzle2 lines = show $ runSeedsThroughMapsAlt (parseSeedsAlt lines) (parseMaps lines)
--puzzle2 lines = show $ parseMaps lines

determineMinimum :: [(Int, Int)] -> Int
determineMinimum = minimum . map fst

runSeedsThroughMapsAlt :: [(Int, Int)] -> [[(Int, Int, Int)]] -> [(Int, Int)]
runSeedsThroughMapsAlt seeds maps = foldl (\s m -> runSeedsThroughMapAlt s m) seeds maps


runSeedsThroughMapAlt :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)]
runSeedsThroughMapAlt [] _ = []
runSeedsThroughMapAlt (s:ss) m = (translateSeed s m) ++ (runSeedsThroughMapAlt ss m)

translateSeed :: (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
translateSeed s [] = s:[]
translateSeed s@(sb,st) ((mb,mt,ma):ms)
    | sb == mb && st == mt           = (sb+ma,st+ma):[]
    | sb == mb && st > mt            = (sb+ma,mt+ma):(translateSeed (mt+1,st) ms)
    | sb == mb && st < mt            = (sb+ma,st+ma):[]
    | sb < mb && st == mt            = (mb+ma,st+ma):(translateSeed (sb,mb-1) ms)
    | sb > mb && st == mt            = (sb+ma,st+ma):[]
    | sb > mb && st < mt             = (sb+ma,st+ma):[]
    | sb < mb && st > mt             = (mb+ma,mt+ma):((translateSeed (sb,mb-1) ms) ++ (translateSeed (mt+1,st) ms))
    | sb > mb && sb <= mt && st > mt = (sb+ma,mt+ma):(translateSeed (mt+1,st) ms)
    | sb < mb && st >= mb && st < mt = (mb+ma,st+ma):(translateSeed (sb,mb-1) ms)
    | otherwise                      = translateSeed s ms

parseSeedsAlt :: [String] -> [(Int, Int)]
parseSeedsAlt = map (\r -> (r!!0,r!!0 + r!!1 - 1)) . chunksOf 2 . map read . tail . words . head
