module Solutions.Day15
( puzzle1
, puzzle2
) where

import Data.List (sortBy, groupBy)
import Data.List.Split (splitOn)
import Data.Char (ord)
import Data.Function (on)

puzzle1 :: [String] -> String
puzzle1 = show . sum . map hashString . parseInstructions

hashString :: String -> Int
hashString s = foldl (\acc c -> ((acc + c) * 17) `mod` 256) 0 (map ord s)

parseInstructions :: [String] -> [String]
parseInstructions lines = splitOn "," (head lines)

puzzle2 :: [String] -> String
puzzle2 = show . getAllBoxesFocusingPower . map parseInstruction . parseInstructions
--puzzle2 lines = show $ calculateBoxFocusingPower 1 $ runThroughBoxInstructions (filter (\i -> hashString (label i) == 0) . map parseInstruction . parseInstructions $ lines) []
--puzzle2 lines = show $  $ groupBy ((==) `on` (\i -> hashString (label i))) $ sortBy (compare `on` (\i -> hashString (label i))) $ map parseInstruction $ parseInstructions lines


getAllBoxesFocusingPower :: [Instruction] -> Int
getAllBoxesFocusingPower instructions = sum $ map getBoxFocusingPower $ map (\g -> ((hashString (label (head g))) + 1, g))  $ groupBy ((==) `on` (\i -> hashString (label i))) $ sortBy (compare `on` (\i -> hashString (label i))) instructions

getBoxFocusingPower :: (Int, [Instruction]) -> Int
getBoxFocusingPower (boxNumber, instructions) = calculateBoxFocusingPower boxNumber (runThroughBoxInstructions instructions [])

calculateBoxFocusingPower :: Int -> [Lens] -> Int
calculateBoxFocusingPower boxNumber lenses = sum $ map (\(n,l) -> boxNumber * n * (lensFocalLength l)) $ zip [1..] lenses

runThroughBoxInstructions :: [Instruction] -> [Lens] -> [Lens]
runThroughBoxInstructions [] lens = lens
runThroughBoxInstructions (i:is) lenses
    | operation i == Remove                     = runThroughBoxInstructions is (filter (\l -> lensLabel l /= label i) lenses)
    | any (\l -> lensLabel l == label i) lenses = runThroughBoxInstructions is (replaceLens lenses (Lens (label i) (focalLength i)))
    | otherwise                                 = runThroughBoxInstructions is (lenses ++ [Lens (label i) (focalLength i)])

replaceLens :: [Lens] -> Lens -> [Lens]
replaceLens [] _ = []
replaceLens (l:ls) newLens
    | lensLabel l == lensLabel newLens = newLens:(replaceLens ls newLens)
    | otherwise                        = l:(replaceLens ls newLens)

parseInstruction :: String -> Instruction
parseInstruction s
    | last s == '-' = Instruction (init s) Remove 0
    | otherwise     = Instruction (init (init s)) Insert (read [(last s)])

data Lens = Lens { lensLabel :: String
                 , lensFocalLength :: Int
                 } deriving (Show, Eq)

data Instruction = Instruction { label :: String
                               , operation :: Operation
                               , focalLength :: Int
                               } deriving (Show, Eq)

data Operation = Insert | Remove deriving (Show, Eq)
