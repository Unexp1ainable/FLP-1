module GeneticAlgorithm where

import Data.List (maximumBy, minimumBy)
import Knapsack
import System.Random

crossoverRate :: Double
crossoverRate = 0.2

mutationRate :: Double
mutationRate = 0.75

reproductionRate :: Double
reproductionRate = 0.3

maxGenerations :: Int
maxGenerations = 200

selectedPopulationSize :: Int
selectedPopulationSize = 1000

generateInitialPopulation :: StdGen -> Int -> Int -> [[Int]]
generateInitialPopulation _ _ 0 = []
generateInitialPopulation seed individualSize populationSize = generateIndividual seed individualSize : generateInitialPopulation newSeed individualSize (populationSize - 1)
  where
    (_, newSeed) = randomR (0, 99999) seed :: (Int, StdGen)

generateIndividual :: StdGen -> Int -> [Int]
generateIndividual seed size = take size $ randomRs (0, 1) seed

determineIndividualFitness :: Weight -> Cost -> [Item] -> [Int] -> Int
determineIndividualFitness maxWeight minCost items individual =
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
tournament (Knapsack maxWeight _ items) (x, y) = if valX > valY then x else y
  where
    valX = determineIndividualFitness maxWeight 0 items x
    valY = determineIndividualFitness maxWeight 0 items y

selectParents :: Knapsack -> [[Int]] -> StdGen -> [[Int]]
selectParents knapsack population seed = [p1, p2]
  where
    (specimen1, newSeed1) = randomR (0, length population - 1) seed
    (specimen2, newSeed2) = randomR (0, length population - 1) newSeed1
    (specimen3, newSeed3) = randomR (0, length population - 1) newSeed2
    (specimen4, _) = randomR (0, length population - 1) newSeed3
    p1 = tournament knapsack (population !! specimen1, population !! specimen2)
    p2 = tournament knapsack (population !! specimen3, population !! specimen4)

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
    newElement = if (xs !! pos) == 0 then 1 else 0

attemptMutateAll :: [[Int]] -> StdGen -> [[Int]]
attemptMutateAll [] _ = []
attemptMutateAll (x : xs) seed = maybeMutated : attemptMutateAll xs newSeed
  where
    (mutationRoll, newSeed) = randomR (0.0, 1.0) seed
    maybeMutated = if mutationRoll <= mutationRate then mutate x newSeed else x

nextGeneration :: Knapsack -> StdGen -> [[Int]] -> [[Int]]
nextGeneration knapsack seed parents = take (length parents) $ nextGenerationF knapsack seed parents (length parents)

nextGenerationF :: Knapsack -> StdGen -> [[Int]] -> Int -> [[Int]]
nextGenerationF knapsack seed parents remaining =
  if remaining <= 0
    then []
    else nextGenerationF knapsack newSeed2 parents (remaining - length generated) ++ generated
  where
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
selectBest (Knapsack maxWeight minCost items) = maximumBy f
  where
    f x y = compare (determineIndividualFitness maxWeight minCost items x) (determineIndividualFitness maxWeight minCost items y)
