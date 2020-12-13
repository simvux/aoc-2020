import Data.List.Split (splitOn)

main = do
  input <- getContents
  let busses = parse input
   in print $ part2 busses

parse :: String -> [(Integer, Integer)]
parse input = 
  case lines input of
    [_,b] -> collect 0 (splitOn "," b)
 where
  collect i (x:xs) = if x == "x" then collect (i+1) xs else (i, read x) : collect (i+1) xs
  collect _ [] = []

part2 :: [(Integer, Integer)] -> Integer
part2 busses = 
   let ((_, id):rest) = busses
    in locate (id, id) rest
 where
  locate (v, _) [] = v
  locate (v, inc) ((time, id):xs) = 
    locate (until (\v -> mod (v + time) id == 0) (+inc) v, inc * id) xs
