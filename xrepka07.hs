module Main where

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

bruteforceUnformatted :: Weight -> Cost -> [Item] -> ([Int], Weight, Cost)
bruteforceUnformatted _ _ [] = ([], 0, 0)
bruteforceUnformatted maxWeight minCost [Item weight cost] = if maxWeight >= weight && minCost <= cost then ([1], weight, cost) else ([0], 0, 0)
bruteforceUnformatted maxWeight minCost ((Item weight cost) : items) =
  if withCost + cost >= withoutCost && withWeight + weight <= maxWeight
    then (1 : withItems, withWeight + weight, withCost + cost)
    else (0 : withoutItems, withoutWeight, withoutCost)
  where
    (withItems, withWeight, withCost) = bruteforceUnformatted (maxWeight - weight) (minCost - cost) items
    (withoutItems, withoutWeight, withoutCost) = bruteforceUnformatted maxWeight minCost items

bruteforce :: Knapsack -> Maybe [Int]
bruteforce (Knapsack maxWeight minCost items) = if summed == 0 then Nothing else Just result
  where
    (result, _, _) = bruteforceUnformatted maxWeight minCost items
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
          Nothing -> print False
