module Knapsack where

import TextEditing (formatItemListStr)

type Weight = Int

type Cost = Int

-- Item of Knapsack
data Item = Item
  { weight :: Weight,
    cost :: Cost
  }

instance Show Item where
  show :: Item -> String
  show (Item weight cost) = "Item {\nweight: " ++ show weight ++ "\ncost: " ++ show cost ++ "\n}\n"

-- Knapsack type
data Knapsack = Knapsack
  { maxWeight :: Weight,
    minCost :: Cost,
    items :: [Item]
  }

instance Show Knapsack where
  show :: Knapsack -> String
  show (Knapsack maxWeight minCost items) = "Knapsack {\nmaxWeight: " ++ show maxWeight ++ "\nminCost: " ++ show minCost ++ "\nitems: [\n" ++ formatItemListStr (show items) ++ "]\n}\n"
