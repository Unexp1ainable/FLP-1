module Main where

import Bruteforce
import GeneticAlgorithm
import KnapsackParser
import System.Environment (getArgs)
import System.Exit (die)
import System.Random

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
          BRUTEFORCE -> formatSolution (bruteforce knapsack)
          OPTIMIZE -> formatSolution (evolution knapsack (mkStdGen 42))
      where
        formatSolution evaluator = case evaluator of
          Just solution -> print solution
          Nothing -> print False
