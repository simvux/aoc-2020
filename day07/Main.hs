import Data.List.Extra
import Data.Maybe

data Rule = Rule {
  bag :: String,
  contain :: [(Int, String)]
} deriving (Show)

main = do
  input <- getContents
  let rules = parse input
  print $ part1 rules
  print $ part2 rules

parse :: String -> [Rule]
parse =
  map parseLine . lines

parseLine :: String -> Rule
parseLine s = 
  case splitOn " contain " s of
    [color, "no other bags."] -> Rule { bag = serializeBag color, contain = [] }
    [color, rules] -> Rule { bag = serializeBag color, contain = parseContain rules }

parseContain :: String -> [(Int, String)]
parseContain =
  map parse' . splitOn ", "
 where 
  parse' (n:' ':xs) = (read [n], serializeBag xs)

part1 :: [Rule] -> Int
part1 rules = 
  length $ filter (canHold "shiny gold bag" rules) rules

part2 :: [Rule] -> Int
part2 rules = holdAmount rules (fetch "shiny gold bag" rules)

fetch :: String -> [Rule] -> Rule
fetch b = fromJust . find (\r -> bag r == b) 

canHold :: String -> [Rule] -> Rule -> Bool
canHold gotbag rules rule = 
  isJust $ find check (contain rule)
 where
  check (_, b) = 
    (b == gotbag) || 
        canHold gotbag rules (fetch b rules)

holdAmount :: [Rule] -> Rule -> Int
holdAmount rules rule =
  foldr count 0 (contain rule)
 where
  count (n, b) acc = 
    acc + n + (n * holdAmount rules (fetch b rules))

serializeBag :: String -> String
serializeBag = dropWhileEnd (\c -> c == '.' || c == 's')
