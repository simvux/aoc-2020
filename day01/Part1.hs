main = do
    input <- getContents
    print $ solve $ map read $ lines input

solve :: [Int] -> Maybe Int
solve input =
  find_map check input
  where 
    check c = 
      find_map (
        \ic -> if ic + c == 2020 
          then Just (c * ic) 
          else Nothing
      ) input

 
    
find_map :: (a -> Maybe b) -> [a] -> Maybe b
find_map f [] = Nothing
find_map f (x:xs) = 
  case f x of
    Just v -> Just v
    Nothing -> find_map f xs
