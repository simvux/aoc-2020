import Debug.Trace (trace)
import Data.List
import Data.List.Split (splitOn)

main = do
  input <- getContents
  let (earliest, busses) = parse input
   in print $ part1 earliest busses

parse :: String -> (Int, [Int])
parse input = 
  case lines input of
    [n,b] -> (read n, collect (splitOn "," b))
 where
  collect (x:xs) = if x == "x" then collect xs else read x : collect xs
  collect [] = []

part1 :: Int -> [Int] -> Int
part1 earliest busses = 
 let timestamps = generateTimestamps busses in
  case find closest timestamps of
    Nothing -> error "answer not found"
    Just (time, busid) -> busid * (time - earliest)
 where
  closest (id, _)
   | id >= earliest = True
   | otherwise = False

generateTimestamps :: [Int] -> [(Int, Int)]
generateTimestamps ids = 
  start 0
 where
  start time = 
    case find (\n -> mod time n == 0) ids of
      Just id -> (time, id) : start (time + 1)
      Nothing -> start (time + 1)
