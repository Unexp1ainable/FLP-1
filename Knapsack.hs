module Knapsack where

import TextEditing (formatItemListStr)

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

instance Show Knapsack where
  show :: Knapsack -> String
  show (Knapsack maxWeight minCost items) = "Knapsack {\nmaxWeight: " ++ show maxWeight ++ "\nminCost: " ++ show minCost ++ "\nitems: [\n" ++ formatItemListStr (show items) ++ "]\n}\n"
