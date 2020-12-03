data Square = Tree | Empty deriving (Eq, Show)

type Map = [Horizontal]

type Horizontal = [Square]

main = do
  input <- getContents
  let map = parse $ lines input
  print $ solve_part_1 map
  print $ solve_part_2 map

solve_part_1 :: Map -> Int
solve_part_1 map =
  count_trees squares
    where
     squares = navigate path 0 map
     path = repeat (3, 1)

solve_part_2 :: Map -> Int
solve_part_2 map = 
  foldr multi 1 paths
  where
    multi path trees = trees * (count_trees $ squares $ repeat path)
    squares p = navigate p 0 map
    paths = [(1,1), (3,1), (5,1), (7,1), (1,2)]

count_trees :: [Square] -> Int
count_trees squares = 
  length $ filter (== Tree) squares

parse :: [String] -> Map
parse lines = 
  map parse_line lines
     
parse_line :: String -> Horizontal
parse_line line =
  map parse_square line
  
parse_square c =
  case c of
    '.' -> Empty
    '#' -> Tree
    _   -> error "invalid square"

navigate :: [(Int, Int)] -> Int -> Map -> [Square]
navigate steps posx map = 
  case steps of
    [] -> []
    ((x,y) : remaining_steps) -> 
      let newmap = drop y map in  
        case newmap of
          [] -> []
          (current_line:_) -> 
            let square = (cycle current_line !! new_x) in
             square : navigate remaining_steps new_x newmap
          where new_x = posx + x
