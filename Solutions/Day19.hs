module Solutions.Day19
( puzzle1
, puzzle2
) where

import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

puzzle1 :: [String] -> String
puzzle1 lines = show . sum . map partScore . filter (isPartAccepted workflows "in") . parseParts $ lines
    where workflows = parseWorkflows lines

partScore :: Part -> Int
partScore = sum . Map.elems

isPartAccepted :: Workflows -> String -> Part -> Bool
isPartAccepted workflows start part
    | start == "A" = True
    | start == "R" = False
    | otherwise    = isPartAccepted workflows (determineNextWorkflow workflows start part) part

determineNextWorkflow :: Workflows -> String -> Part -> String
determineNextWorkflow workflows start part = followFirstRelevantRule r part
    where Workflow _ r = fromJust (Map.lookup start workflows)

followFirstRelevantRule :: [Rule] -> Part -> String
followFirstRelevantRule ((Rule category condition threshold destination):rs) part
    | compare (fromJust (Map.lookup category part)) threshold == condition = destination
    | otherwise                                                 = followFirstRelevantRule rs part

parseWorkflows :: [String] -> Workflows
parseWorkflows = Map.fromList . map parseWorkflow . fst . break (==[])

parseWorkflow :: String -> (String, Workflow)
parseWorkflow line = (name, Workflow name rules)
    where initialSplit = splitOn "{" line
          name = head initialSplit
          rules = parseRules . init . last $ initialSplit 

parseRules :: String -> [Rule]
parseRules = map parseRule . splitOn ","

parseRule :: String -> Rule
parseRule rule
    | ':' `elem` rule = parseComplexRule rule
    | otherwise       = parseSimpleRule rule

parseSimpleRule :: String -> Rule
parseSimpleRule dest = Rule X GT (-1) dest

parseComplexRule :: String -> Rule
parseComplexRule rule = Rule category condition threshold destination
    where category = read [toUpper . head $ rule]
          condition = getCondition rule
          threshold = read . tail . tail . head . splitOn ":" $ rule
          destination = last . splitOn ":" $ rule

getCondition :: String -> Ordering
getCondition rule
    | '<' `elem` rule = LT
    | otherwise       = GT

parseParts :: [String] -> [Part]
parseParts = map parsePart . tail . snd . break (==[])

parsePart :: String -> Part
parsePart line = Map.fromList [(X, xRating), (M, mRating), (A, aRating), (S, sRating)]
    where removeBraces = init . tail $ line
          intoRatings = splitOn "," removeBraces
          xRating = read . last . splitOn "=" $ intoRatings!!0
          mRating = read . last . splitOn "=" $ intoRatings!!1
          aRating = read . last . splitOn "=" $ intoRatings!!2
          sRating = read . last . splitOn "=" $ intoRatings!!3

type Part = Map.Map Category Int

type Workflows = Map.Map String Workflow

data Workflow = Workflow { name :: String
                         , rules :: [Rule]
                         } deriving (Show)

data Rule = Rule { categoryToCheck :: Category
                 , conditionType :: Ordering
                 , thresholdForCondition :: Int
                 , destinationWorkflow :: String
                 } deriving (Show)

data Category = X | M | A | S deriving (Show, Read, Eq, Ord)

puzzle2 :: [String] -> String
puzzle2 lines = show $ narrowDownPartRange workflows (rules (fromJust (Map.lookup "in" workflows))) (Map.fromList [(X,(1,4000)), (M,(1,4000)), (A,(1,4000)), (S,(1,4000))])
    where workflows = parseWorkflows lines

narrowDownPartRange :: Workflows -> [Rule] -> PartRange -> Int
narrowDownPartRange _ [] _ = 0
narrowDownPartRange workflows (r@(Rule _ _ _ destination):rs) range
    | destination == "R" = narrowDownPartRange workflows rs narrowedInverse
    | destination == "A" = (combosInRange narrowed) + (narrowDownPartRange workflows rs narrowedInverse)
    | otherwise          = (narrowDownPartRange workflows (rules (fromJust (Map.lookup destination workflows))) narrowed) + (narrowDownPartRange workflows rs narrowedInverse)
    where narrowed        = narrowDownRange r range
          narrowedInverse = narrowDownRangeInverse r range

narrowDownRange :: Rule -> PartRange -> PartRange
narrowDownRange (Rule category condition threshold destination) range
    | condition == LT = Map.insert category (rangeBottom, (min (threshold-1) rangeTop)) range
    | otherwise       = Map.insert category ((max (threshold+1) rangeBottom), rangeTop) range
    where (rangeBottom,rangeTop) = fromJust (Map.lookup category range)

narrowDownRangeInverse :: Rule -> PartRange -> PartRange
narrowDownRangeInverse (Rule category condition threshold destination) range
    | condition == GT = Map.insert category (rangeBottom, (min threshold rangeTop)) range
    | otherwise       = Map.insert category ((max threshold rangeBottom), rangeTop) range
    where (rangeBottom,rangeTop) = fromJust (Map.lookup category range)

combosInRange :: PartRange -> Int
combosInRange = foldl1 (*) . map rangeSize . Map.elems

rangeSize :: (Int,Int) -> Int
rangeSize (bottom,top) = max 0 (top - bottom + 1)

type PartRange = Map.Map Category (Int,Int)
