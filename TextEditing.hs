module TextEditing where

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [_] = []
removeLast (x : xs) = x : removeLast xs

-- strip :: [a] -> [a]
