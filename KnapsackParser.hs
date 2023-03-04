{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module KnapsackParser where

import Knapsack
import Text.Parsec
import Text.Parsec.String

parseItemWeight :: Parser Int
parseItemWeight = do
  spaces
  _ <- string "weight: "
  weight <- many1 digit
  _ <- string "\n"
  return (read weight)

parseItemCost :: Parser Int
parseItemCost = do
  spaces
  _ <- string "cost: "
  cost <- many1 digit
  _ <- string "\n"
  return (read cost)

parseItem :: Parser Item
parseItem = do
  spaces
  _ <- string "Item {\n"
  try
    ( do
        weight <- parseItemWeight
        cost <- parseItemCost
        _ <- string "}\n"
        spaces
        return (Item weight cost)
    )
    <|> try
      ( do
          cost <- parseItemCost
          weight <- parseItemWeight
          _ <- string "}\n"
          spaces
          return (Item weight cost)
      )

parseKnapsackMaxWeight :: Parser Int
parseKnapsackMaxWeight = do
  spaces
  _ <- string "maxWeight: "
  maxWeight <- many1 digit
  _ <- string "\n"
  return (read maxWeight)

parseKnapsackMinCost :: Parser Int
parseKnapsackMinCost = do
  spaces
  _ <- string "minCost: "
  minCost <- many1 digit
  _ <- string "\n"
  return (read minCost)

parseKnapsackItems :: Parser [Item]
parseKnapsackItems = do
  spaces
  _ <- string "items: ["
  items <- many1 parseItem
  spaces
  _ <- string "]"
  return items

parseKnapsack :: Parser Knapsack
parseKnapsack = do
  spaces
  _ <- string "Knapsack {\n"
  weight <- parseKnapsackMaxWeight
  minCost <- parseKnapsackMinCost
  items <- parseKnapsackItems
  spaces
  _ <- string "}\n"
  return (Knapsack weight minCost items)

knapsackParser :: String -> Either ParseError Knapsack
knapsackParser = parse parseKnapsack ""
