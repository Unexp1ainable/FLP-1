module Knapsack where

import TextEditing

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
