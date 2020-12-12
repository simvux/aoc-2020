main = do
  input <- getContents
  let route = lines input
   in do
    putStrLn $ "part 1: " <> show (part1 route)
    putStrLn $ "part 2: " <> show (part2 route)

part1 :: [String] -> Int
part1 input = 
  let (x, y, _) = foldl move (0, 0, 'E') input
   in abs x + abs y
 where 
  move (x, y, c) ('N':n) = (x, y - read n, c)
  move (x, y, c) ('S':n) = (x, y + read n, c)
  move (x, y, c) ('E':n) = (x + read n, y, c)
  move (x, y, c) ('W':n) = (x - read n, y, c)
  move (x, y, c) ('F':n) = move (x, y, c) (c:n)
  move (x, y, c) (d:"90") = (x, y, turn c d 1)
  move (x, y, c) (d:"180") = (x, y, turn c d 2)
  move (x, y, c) (d:"270") = (x, y, turn c d 3)

turn :: Char -> Char -> Int -> Char
turn a b 1 = turnonce a b
turn a b n = turn (turnonce a b) b (n-1)

turnonce :: Char -> Char -> Char
turnonce 'W' 'L' = 'S'
turnonce 'S' 'L' = 'E'
turnonce 'E' 'L' = 'N'
turnonce 'N' 'L' = 'W'
turnonce 'W' 'R' = 'N'
turnonce 'N' 'R' = 'E'
turnonce 'E' 'R' = 'S'
turnonce 'S' 'R' = 'W'

data Nav = Nav {
  x :: Int, y :: Int, wx :: Int, wy :: Int
} deriving (Show)

part2 :: [String] -> Int
part2 input = 
  let nav = foldl move start input
   in abs (x nav) + abs (y nav)
 where
  start = Nav { x = 0, y = 0, wx = 10, wy = -1 }
  move nav ('N':n) = nav { wy = wy nav - read n }
  move nav ('S':n) = nav { wy = wy nav + read n }
  move nav ('E':n) = nav { wx = wx nav + read n }
  move nav ('W':n) = nav { wx = wx nav - read n }
  move nav ('F':n) = nav { x = x nav + (wx nav * read n), y = y nav + (wy nav * read n) }
  move nav (_:"180") = nav { wx = negate (wx nav), wy = negate (wy nav) }
  move nav ('L':"90") = nav { wy = negate (wx nav), wx = wy nav } 
  move nav ('R':"90") = nav { wx = negate (wy nav), wy = wx nav }
  move nav (d:"270") = foldl move nav (replicate 3 (d:"90"))
