module Main where

import Data.Bifunctor qualified
import Knapsack
import KnapsackParser
import System.Environment (getArgs)
import System.Exit (die)

--------------------------------------
data Mode = ECHO | BRUTEFORCE | OPTIMIZE deriving (Enum, Show)

-- Argument parsing
parseArgs :: [String] -> Maybe Mode
parseArgs [] = Nothing
parseArgs [x]
  | x == "-i" = Just ECHO
  | x == "-b" = Just BRUTEFORCE
  | x == "-o" = Just OPTIMIZE
  | otherwise = Nothing
parseArgs _ = Nothing

enumerateCombinations :: Int -> [[Int]]
enumerateCombinations 0 = []
enumerateCombinations 1 = [[0], [1]]
enumerateCombinations len = [0 : x | x <- rest] ++ [1 : x | x <- rest]
  where
    rest = enumerateCombinations (len - 1)

sumAccordingToFlags :: [Item] -> [Int] -> (Weight, Cost)
sumAccordingToFlags [] [] = (0, 0)
sumAccordingToFlags ((Item weight cost) : items) (flag : flags) =
  if flag == 1
    then Data.Bifunctor.bimap (weight +) (cost +) rest -- that was language server, not me
    else rest
  where
    rest = sumAccordingToFlags items flags
sumAccordingToFlags _ _ = (0, 0)

selectBest :: Weight -> Cost -> [([Int], Weight, Cost)] -> [Int]
selectBest maxWeight minCost items =
  fst $
    foldr
      ( \(flags, weight, cost) (tmpMaxflags, tmpMaxCost) ->
          if weight <= maxWeight && cost >= minCost && cost > tmpMaxCost then (flags, cost) else (tmpMaxflags, tmpMaxCost)
      )
      ([0], 0)
      items

bruteforceUnformatted :: Knapsack -> [Int]
bruteforceUnformatted (Knapsack maxWeight minCost items) =
  selectBest maxWeight minCost $
    [ (flags, weight, cost)
      | flags <- allFlagCombinations,
        let (weight, cost) = sumAccordingToFlags items flags,
        weight <= maxWeight,
        cost >= minCost
    ]
  where
    allFlagCombinations = enumerateCombinations $ length items

bruteforce :: Knapsack -> Maybe [Int]
bruteforce knapsack = if summed == 0 then Nothing else Just result
  where
    result = bruteforceUnformatted knapsack
    summed = sum result

------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let maybeMode = parseArgs args
  case maybeMode of
    Nothing -> die "Invalid arguments."
    Just mode -> do
      input <- getContents
      case knapsackParser input of
        Left err -> putStrLn $ "Error: " ++ show err
        Right knapsack -> case mode of
          ECHO -> print knapsack
          BRUTEFORCE -> formattedBruteforce knapsack
          OPTIMIZE -> print knapsack
      where
        formattedBruteforce knapsack = case bruteforce knapsack of
          Just solution -> print solution
          Nothing -> print "False"
