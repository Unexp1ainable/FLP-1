module GeneticAlgorithm where

import Control.Monad.IO.Class qualified
import GHC.Builtin.PrimOps (primOpOkForSpeculation)
import Knapsack
import System.Exit
import System.Random

crossoverRate :: Double
crossoverRate = 0.2

generateInitialPopulation :: Int -> Int -> [IO [Int]]
generateInitialPopulation _ 0 = []
generateInitialPopulation individualSize populationSize = do generateIndividual individualSize : generateInitialPopulation individualSize (populationSize - 1)

generateIndividual :: Int -> IO [Int]
generateIndividual size = do take 10 . randomRs (0, 1) <$> newStdGen

determineIndividualFitness :: Knapsack -> [Int] -> Int
determineIndividualFitness (Knapsack maxWeight minCost items) individual =
  if totalWeight > maxWeight then 0 else totalCost
  where
    (totalWeight, totalCost) = (0, 0)

itemStats :: [Item] -> [Int] -> (Weight, Cost)
itemStats [] _ = (0, 0)
itemStats _ [] = (0, 0)
itemStats (item : items) (selection : rest) = (itemWeight + restWeight, itemCost + restCost)
  where
    (restWeight, restCost) = itemStats items rest
    itemWeight = if selection == 1 then weight item else 0
    itemCost = if selection == 1 then cost item else 0

makeTuple :: [[Int]] -> ([Int], [Int])
makeTuple [a, b] = (a, b)
makeTuple _ = ([], [])

selectParents :: [[Int]] -> StdGen -> ([Int], [Int])
selectParents population seed = pairs !! randomIndex
  where
    pairs = [(x, y) | x <- population, y <- population, x /= y]
    (randomIndex, _) = randomR (0, length pairs) seed

crossover :: [Int] -> [Int] -> ([Int], [Int])

crossover
