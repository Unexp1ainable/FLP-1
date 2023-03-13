module GeneticAlgorithm where

import Data.List (minimumBy)
import Knapsack
import System.Random

crossoverRate :: Double
crossoverRate = 0.2

mutationRate :: Double
mutationRate = 0.01

reproductionRate :: Double
reproductionRate = 0.3

maxGenerations :: Int
maxGenerations = 10

selectedPopulationSize :: Int
selectedPopulationSize = 6

generateInitialPopulation :: StdGen -> Int -> Int -> [[Int]]
generateInitialPopulation _ _ 0 = []
generateInitialPopulation seed individualSize populationSize = generateIndividual seed individualSize : generateInitialPopulation newSeed individualSize (populationSize - 1)
  where
    (_, newSeed) = randomR (0, 99999) seed :: (Int, StdGen)

generateIndividual :: StdGen -> Int -> [Int]
generateIndividual seed size = take size $ randomRs (0, 1) seed

determineIndividualFitness :: Knapsack -> [Int] -> Int
determineIndividualFitness (Knapsack maxWeight minCost items) individual =
  if totalWeight > maxWeight || totalCost < minCost then 0 else totalCost
  where
    (totalWeight, totalCost) = itemStats items individual

itemStats :: [Item] -> [Int] -> (Weight, Cost)
itemStats [] _ = (0, 0)
itemStats _ [] = (0, 0)
itemStats (item : items) (selection : rest) = (itemWeight + restWeight, itemCost + restCost)
  where
    (restWeight, restCost) = itemStats items rest
    itemWeight = if selection == 1 then weight item else 0
    itemCost = if selection == 1 then cost item else 0

tournament :: Knapsack -> ([Int], [Int]) -> [Int]
tournament knapsack (x, y) = if valX > valY then x else y
  where
    valX = determineIndividualFitness knapsack x
    valY = determineIndividualFitness knapsack y

selectParents :: Knapsack -> [[Int]] -> StdGen -> [[Int]]
selectParents knapsack population seed = [p1, p2]
  where
    pairs = [(x, y) | x <- population, y <- population, x /= y]
    (randomIndex1, newSeed) = randomR (0, length pairs - 1) seed
    (randomIndex2, _) = randomR (0, length pairs - 1) newSeed
    p1 = tournament knapsack (pairs !! randomIndex1)
    p2 = tournament knapsack (pairs !! randomIndex2)

crossover :: [Int] -> [Int] -> [[Int]]
crossover [] _ = []
crossover _ [] = []
crossover a b = [first, second]
  where
    len = length a
    half = len `div` 2
    first = take half a ++ drop half b
    second = take half b ++ drop half a

mutate :: [Int] -> StdGen -> [Int]
mutate [] _ = []
mutate xs seed = take pos xs ++ [newElement] ++ drop (pos + 1) xs
  where
    (pos, _) = randomR (0, length xs - 1) seed
    newElement = if xs !! pos == 0 then 1 else 0

attemptMutateAll :: [[Int]] -> StdGen -> [[Int]]
attemptMutateAll [] _ = []
attemptMutateAll (x : xs) seed = maybeMutated : attemptMutateAll xs newSeed
  where
    (mutationRoll, newSeed) = randomR (0.0, 1.0) seed
    maybeMutated = if mutationRoll <= mutationRate then mutate x newSeed else x

nextGeneration :: Knapsack -> StdGen -> [[Int]] -> [[Int]]
nextGeneration knapsack seed parents = if length generated >= initialLength then take initialLength generated else nextGeneration knapsack seed parents ++ generated
  where
    initialLength = length parents
    newParents = selectParents knapsack parents seed
    (reproductionRoll, newSeed1) = randomR (0.0, 1.0) seed
    children = if reproductionRoll <= reproductionRate then newParents else []
    (crossoverRoll, newSeed2) = randomR (0.0, 1.0) newSeed1
    crossovered = if crossoverRoll <= crossoverRate then crossover (head newParents) (newParents !! 1) else newParents
    mutated = attemptMutateAll crossovered newSeed2
    generated = if null children then mutated else children

evolutionStep :: Int -> Knapsack -> StdGen -> [[Int]] -> [[Int]]
evolutionStep 0 _ _ population = population
evolutionStep i knapsack seed population = evolutionStep (i - 1) knapsack newSeed (nextGeneration knapsack newSeed population)
  where
    (_, newSeed) = randomR (0, 99999) seed :: (Int, StdGen)

evolution :: Knapsack -> StdGen -> [Int]
evolution knapsack seed = selectBest knapsack $ evolutionStep maxGenerations knapsack seed initialPopulation
  where
    initialPopulation = generateInitialPopulation seed (length (items knapsack)) selectedPopulationSize

selectBest :: Knapsack -> [[Int]] -> [Int]
selectBest knapsack = minimumBy (flip f)
  where
    f x y = compare (determineIndividualFitness knapsack x) (determineIndividualFitness knapsack y)
