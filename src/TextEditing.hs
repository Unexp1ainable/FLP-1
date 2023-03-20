----------------------------------
-- FLP - Functional project     --
-- Author: Samuel Repka         --
-- Login: xrepka07              --
-- Year: 2023                   --
----------------------------------
-- Helper functions for text formating

module TextEditing where

-- remove last item from list
-- (used for removing last '\n' from Item list)
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
      | x == '[' || x == ']' || x == ',' = acc -- remove unnecessary characters
      | x == '\n' = "\n\t" ++ acc -- add indentation
      | otherwise = x : acc
