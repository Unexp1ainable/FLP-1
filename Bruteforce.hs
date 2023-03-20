module Bruteforce where

import Knapsack (Cost, Item (Item), Knapsack (Knapsack), Weight)

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
