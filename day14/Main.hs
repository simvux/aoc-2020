import Data.List.Split (splitOn)
import Data.Bits
import qualified Data.HashMap as HM
import Data.List.Extra (replace)
import Data.List
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, fromJust)
import Numeric    (readInt)

data Entity = IncludeMask String | Write Int Int deriving (Show)

data Bit = Zero | One | None deriving (Show)

bit 'X' = None
bit '0' = Zero
bit '1' = One

main = do
  input <- getContents
  let ast = parse input
   in do
    print $ part1 ast

parse :: String -> [Entity]
parse = map parseEntity . lines

parseEntity :: String -> Entity
parseEntity s = 
  case take 3 s of
    "mas" -> findMask
    "mem" -> findWrite
 where
  findMask  = IncludeMask $ drop 7 s
  findWrite = 
    case splitOn "=" s of
      [instr, v] -> 
        let address = read $ splitOn "[" (head $ splitOn "]" instr) !! 1
         in Write address (read v)

data Emu = Emu {
  memory :: HM.Map Int Int,
  mask   :: String
}

newEmu :: Emu
newEmu = Emu {
  memory = HM.empty,
  mask = replicate 36 'X'
}

part1 :: [Entity] -> Int
part1 =
  run newEmu
 where
  run emu [] = sum $ HM.elems $ memory emu 
  run emu (IncludeMask mask:xs) = run emu{mask = mask} xs
  run emu (Write address value:xs) =
    run emu{
     memory = HM.insert address (parseMask value (mask emu)) (memory emu)
    } xs


parseMask :: Int -> String -> Int
parseMask value mask =
  let or_op = fromJust $ readBin $ replace "X" "0" mask in
  let and_op = fromJust $ readBin $ replace "X" "1" mask in
    (value .|. or_op) .&. and_op

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
