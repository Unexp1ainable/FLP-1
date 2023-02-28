module Main where

import System.Environment (getArgs)
import System.Exit (die)

-- Item of Knapsack
data Item = Item
  { weight :: Int,
    cost :: Int
  }

instance Show Item where
  show :: Item -> String
  show (Item weight cost) = "Item {\nweight: " ++ show weight ++ "\ncost: " ++ show cost ++ "\n}\n"

-- Knapsack type
data Knapsack = Knapsack
  { maxWeight :: Int,
    minCost :: Int,
    items :: [Item]
  }

-- helper functions for formatting output
removeLast :: [a] -> [a]
removeLast [] = []
removeLast [_] = []
removeLast (x : xs) = x : removeLast xs

-- change format of default Show string
-- remove last \t and prepend additional \t
formatItemListStr :: String -> String
formatItemListStr = removeLast . (++) "\t" . foldr format ""
  where
    format :: Char -> String -> String
    format x acc
      | x == '[' || x == ']' || x == ',' = acc
      | x == '\n' = "\n\t" ++ acc
      | otherwise = x : acc

instance Show Knapsack where
  show :: Knapsack -> String
  show (Knapsack maxWeight minCost items) = "Knapsack {\nmaxWeight: " ++ show maxWeight ++ "\nminCost: " ++ show minCost ++ "\nitems: [\n" ++ formatItemListStr (show items) ++ "]\n}\n"

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

-- Deserialize Item

-- Deserialize Knapsack

------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let mode = parseArgs args
  maybe (die "Invalid arguments.") print mode

  putStrLn "Done"
