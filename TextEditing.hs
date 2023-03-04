module TextEditing where

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
      | x == '[' || x == ']' || x == ',' = acc
      | x == '\n' = "\n\t" ++ acc
      | otherwise = x : acc
