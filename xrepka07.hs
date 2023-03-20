----------------------------------
-- FLP - Functional project     --
-- Author: Samuel Repka         --
-- Login: xrepka07              --
-- Year: 2023                   --
----------------------------------
module Main where

import Bruteforce (bruteforce)
import GeneticAlgorithm (evolution)
import KnapsackParser (knapsackParser)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random (mkStdGen)

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
  -- check argument parsing result
  case maybeMode of
    Nothing -> die "Invalid arguments."
    Just mode -> do
      -- parse input knapsack
      input <- getContents
      case knapsackParser input of
        Left err -> putStrLn $ "Error: " ++ show err
        Right knapsack -> case mode of
          -- proceed to solution
          ECHO -> print knapsack
          BRUTEFORCE -> formatSolution (bruteforce knapsack)
          OPTIMIZE -> formatSolution (evolution knapsack (mkStdGen 42))
      where
        formatSolution evaluator = case evaluator of
          Just solution -> print solution
          Nothing -> print False
