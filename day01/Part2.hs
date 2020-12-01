main = do
  input <- getContents
  print $ solve $ map read $ lines input


solve :: [Int] -> Maybe Int
solve input =
  find_map doublecheck input
  where 
    doublecheck c = 
      find_map (
        \ic -> 
          find_map (
            \iic -> if iic + ic + c == 2020 then Just (iic * ic * c) else Nothing
          ) input
      ) input
 
find_map :: (a -> Maybe b) -> [a] -> Maybe b
find_map f [] = Nothing
find_map f (x:xs) = 
  case f x of
    Just v -> Just v
    Nothing -> find_map f xs
