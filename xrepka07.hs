module Main where

import Knapsack
import KnapsackParser
import System.Environment (getArgs)
import System.Exit (die)

--------------------------------------
data Mode = ECHO | BRUTEFORCE | OPTIMALIZE deriving (Enum, Show)

-- Argument parsing
parseArgs :: [String] -> Maybe Mode
parseArgs [] = Nothing
parseArgs [x]
  | x == "-i" = Just ECHO
  | x == "-b" = Just BRUTEFORCE
  | x == "-o" = Just OPTIMALIZE
  | otherwise = Nothing
parseArgs _ = Nothing

------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let mode = parseArgs args
  maybe (die "Invalid arguments.") print mode

  input <- getContents
  case knapsackParser input of
    Left err -> putStrLn $ "Error: " ++ show err
    Right result -> putStrLn $ "Parsed input: \n" ++ show result
