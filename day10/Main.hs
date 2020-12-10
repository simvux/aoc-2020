import qualified Data.HashMap as HM

main = do
  input <- getContents
  let numbers = parse input
  let builtin = maximum numbers + 3
  putStrLn $ "builtin: " <> show builtin
  putStrLn $ "part 1: " <> show (part1 (0 : numbers ++ [builtin]))
  putStrLn $ "part 2: " <> show (part2 numbers builtin)

type Adapters = [Int]

data Differs = Differs {
    one   :: Int,
    two   :: Int,
    three :: Int
} deriving (Show)

emptyDiffer = Differs {
  one = 0,
  two = 0,
  three = 0
}

parse :: String -> Adapters
parse = map read . lines

part1 :: Adapters -> Int
part1 adapters = 
  let differs = walker emptyDiffer adapters
   in one differs * three differs
 where
  walker differs [_] = differs
  walker differs (x:xs) = walker (walk differs x) xs
  walk differs adapter = 
    case pickAdapter adapter adapters - adapter of
      1 -> differs { one = one differs + 1 }
      2 -> differs { two = two differs + 1 }
      3 -> differs { three = three differs + 1 }

pickAdapter :: Int -> [Int] -> Int
pickAdapter a = minimum . compatibleAdapters a

compatibleAdapters :: Int -> [Int] -> [Int]
compatibleAdapters with = 
  filter (\a -> a > with && a < (with + 4)) 

part2 :: Adapters -> Int -> Int
part2 adapters builtin = 
  fst $ countAll HM.empty 0 0
 where
  countAll cache n acc
    | (n + 1) == builtin || (n + 2) == builtin || (n + 3) == builtin = (acc + 1, cache)
    | otherwise = 
      case HM.lookup n cache of
        Just added -> (acc + added, cache)
        Nothing -> uncached cache n acc
  uncached cache n acc =
    case compatibleAdapters n adapters of
      [] -> (acc, cache)
      other -> 
        foldr (
          \n (acc, c) -> 
            let (added, cache) = countAll c n 0
              in (acc + added, HM.insert n added cache)
        ) (acc, cache) other

type Cache = HM.Map Int Int
