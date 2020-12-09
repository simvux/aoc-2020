import Data.List
import Data.Maybe

main = do
  input <- getContents
  let numbs = map read $ lines input
   in do
    let invalid = part1 numbs
    print invalid
    print $ part2 numbs invalid

premble = 25

part1 :: [Int] -> Int
part1 n = head $ findInvalids n (drop premble n)

part2 :: [Int] -> Int -> Int
part2 n invalid = 
 let contiguous = solve [] n
  in maximum contiguous + minimum contiguous
 where 
  solve prev ptr
   | sum prev == invalid = prev
   | sum prev > invalid = solve (drop 1 prev) ptr
   | otherwise = 
     case ptr of
      (x:xs) -> solve (prev ++ [x]) xs
      _ -> error "impossible input"

findInvalids :: [Int] -> [Int] -> [Int]
findInvalids all ptr = 
  case ptr of
    [] -> []
    (x:xs) -> 
      if not $ hasSum all x
        then x : findInvalids (drop 1 all) xs
        else findInvalids (drop 1 all) xs

hasSum :: [Int] -> Int -> Bool
hasSum all n = 
  any match all
 where 
  match a =
   isJust $ find (\b -> a + b == n) all
