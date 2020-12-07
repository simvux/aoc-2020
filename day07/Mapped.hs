{-# LANGUAGE OverloadedStrings  #-}

import Data.List.Extra
import Data.Maybe
import qualified Data.HashMap as HM
import qualified Data.Text as T

type Rules = HM.Map T.Text [(Int, T.Text)]

main = do
  input <- getContents
  let rules = parse $ T.pack input
  print $ part1 rules
  print $ part2 rules

parse :: T.Text -> Rules
parse =
  HM.fromList . map parseLine . T.lines

parseLine :: T.Text -> (T.Text, [(Int, T.Text)])
parseLine s = 
  case T.splitOn " contain " s of
    [color, "no other bags."] -> (serializeBag color, [])
    [color, rules] -> (serializeBag color, parseContain rules)

parseContain :: T.Text -> [(Int, T.Text)]
parseContain =
  map parse' . T.splitOn ", "
 where 
  parse' s = (read [T.head s], serializeBag $ T.drop 2 s)

part1 :: Rules -> Int
part1 rules = 
  HM.size $ HM.filter (canHold "shiny gold" rules) rules

part2 :: Rules -> Int
part2 rules = holdAmount rules (fetch "shiny gold" rules)

fetch :: T.Text -> Rules -> [(Int, T.Text)]
fetch b m = fromJust $ HM.lookup b m

canHold :: T.Text -> Rules -> [(Int, T.Text)] -> Bool
canHold gotbag rules rule = 
  isJust $ find check rule
 where
  check (_, b) = 
    (b == gotbag) || 
        canHold gotbag rules (fetch b rules)

holdAmount :: Rules -> [(Int, T.Text)] -> Int
holdAmount rules =
  foldr count 0
 where
  count (n, b) acc = 
    acc + n + (n * holdAmount rules (fetch b rules))

serializeBag :: T.Text -> T.Text
serializeBag = T.replace (T.pack " bag") (T.pack "") . T.dropWhileEnd (\c -> c == '.' || c == 's')
