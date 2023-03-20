----------------------------------
-- FLP - Functional project     --
-- Author: Samuel Repka         --
-- Login: xrepka07              --
-- Year: 2023                   --
----------------------------------
module KnapsackParser where

import Knapsack (Item (Item), Knapsack (Knapsack))
import Text.Parsec
  ( ParseError,
    digit,
    many1,
    parse,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)

parseItemWeight :: Parser Int
parseItemWeight = do
  spaces
  _ <- string "weight: "
  weight <- many1 digit
  spaces
  return (read weight)

parseItemCost :: Parser Int
parseItemCost = do
  spaces
  _ <- string "cost: "
  cost <- many1 digit
  spaces
  return (read cost)

parseItem :: Parser Item
parseItem = do
  spaces
  _ <- string "Item {"
  spaces
  try
    ( do
        weight <- parseItemWeight
        cost <- parseItemCost
        _ <- string "}"
        spaces
        return (Item weight cost)
    )
    <|> try
      ( do
          cost <- parseItemCost
          weight <- parseItemWeight
          _ <- string "}"
          spaces
          return (Item weight cost)
      )

parseKnapsackMaxWeight :: Parser Int
parseKnapsackMaxWeight = do
  spaces
  _ <- string "maxWeight: "
  maxWeight <- many1 digit
  spaces
  return (read maxWeight)

parseKnapsackMinCost :: Parser Int
parseKnapsackMinCost = do
  spaces
  _ <- string "minCost: "
  minCost <- many1 digit
  spaces
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
  _ <- string "Knapsack {"
  spaces
  weight <- parseKnapsackMaxWeight
  minCost <- parseKnapsackMinCost
  items <- parseKnapsackItems
  spaces
  _ <- string "}"
  spaces
  return (Knapsack weight minCost items)

knapsackParser :: String -> Either ParseError Knapsack
knapsackParser = parse parseKnapsack ""
