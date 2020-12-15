import qualified Data.HashMap as HM
import Data.List.Split (splitOn)

main = do
  input <- getContents
  let starting_numbers = map read $ splitOn "," input
  print $ part1 starting_numbers
  print $ part2 starting_numbers

part1 :: [Int] -> Int
part1 = onYear 2020

part2 :: [Int] -> Int
part2 = onYear 30000000

type Spoken = HM.Map Int Int
  
onYear :: Int -> [Int] -> Int
onYear year input = 
 let ((spoken, said), round) = ugly_init_hack
   in go (spoken, said) round
 where
  go (spoken, said) round
   | round == year = said
   | otherwise = 
      go (play spoken said round) (round + 1)
  ugly_init_hack = 
    let init = play HM.empty (head input) 1 in 
      foldl (\((spoken, _), round) n -> (play spoken n round, round + 1) ) (init, 2) (drop 1 input)

play :: Spoken -> Int -> Int -> (Spoken, Int)
play spoken said round = 
  case HM.lookup said spoken of
    Nothing -> (new, 0)
    Just old_round -> (new, round - old_round)
 where new = HM.insert said round spoken
