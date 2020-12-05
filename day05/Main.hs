import Data.List (findIndex)

data RowHint = F | B
data ColHint = L | R

data Seating = Seating {
    row :: [RowHint],
    col :: [ColHint]
}

main = do
  input <- getContents
  let seating = parse input
   in do
    print $ part1 seating
    print $ part2 seating

part1 :: [Seating] -> Int
part1 seating = 
 maximum $ map seatID seating

part2 :: [Seating] -> Maybe Int
part2 seating =
 let occu = map seatID seating in 
 let taken = map (`elem` occu) [0..1024]
  in (+1) <$> findIndex (\(seat, next) -> seat && not next) (pairs taken)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

trySeat :: (Int, Int, Int) -> [Int] -> Bool
trySeat (prior, seat, next) taken = 
  elem seat taken && elem prior taken && elem next taken

parse :: String -> [Seating]
parse = map parseLine . lines

parseLine :: String -> Seating
parseLine raw = parse' raw $ Seating { row = [], col = [] }
  where 
   parse' [] s = s
   parse' (c:xs) s = parse' xs $ parseChar c s

parseChar :: Char -> Seating -> Seating
parseChar c s = 
  case c of 
    'F' -> s { row = row s ++ [F] }
    'B' -> s { row = row s ++ [B] }
    'L' -> s { col = col s ++ [L] }
    'R' -> s { col = col s ++ [R] }

seatID :: Seating -> Int
seatID s = 
  let r = findRow (row s)
   in (r * 8) + findCol (col s)
  
findRow :: [RowHint] -> Int
findRow = lower 0 127
  where 
   lower f _ [F] = f
   lower _ t [B] = t
   lower f t (F:xs) = lower f ((t - div (t - f) 2) - 1) xs
   lower f t (B:xs) = lower (t - div (t - f) 2) t xs

findCol :: [ColHint] -> Int
findCol = lower 0 7
  where
   lower f _ [L] = f
   lower _ t [R] = t
   lower f t (L:xs) = lower f ((t - div (t - f) 2) - 1) xs
   lower f t (R:xs) = lower (t - div (t - f) 2) t xs
