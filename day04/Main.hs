import Data.List.Split (splitOn, splitOneOf)
import Data.List (isInfixOf)
import Data.Maybe
import Debug.Trace (trace)

type RawPassports = [[String]]

main = do
  input <- getContents
  let passports = map (splitOneOf "\n ") $ splitOn "\n\n" input
  putStrLn ("part 1: " <> (show $ solve_1 passports))
  putStrLn ("part 2: " <> (show $ solve_2 passports))

solve_1 :: RawPassports -> Int
solve_1 pps = 
  length $ filter valid pps
  where 
    valid rpp = 
      let pp = parse rpp
       in (length $ filter not_cid pp) > 6

not_cid (k, v) = k /= "cid"

solve_2 :: RawPassports -> Int
solve_2 pps =
  length $ filter valid pps
  where
    valid rpp = 
      let pp = parse rpp
       in (length $ filter (field_valid) $ filter not_cid pp) > 6
    field_valid (k, v) = 
      case k of
        "byr" -> 
          on_num (\n -> n > 1919 && n < 2003) v
        "iyr" -> 
          on_num (\n -> n > 2009 && n < 2021) v
        "eyr" -> 
          on_num (\n -> n > 2019 && n < 2031) v
        "hgt" ->
          case reads v of
            [(n, unit)] -> 
              case unit of
                "cm" -> n > 149 && n < 194
                "in" -> n > 58 && n < 77
                other -> False
            other -> False
        "hcl" ->
          (v !! 0) == '#' && (length $ filter (\c -> isInfixOf [c] "1234567890abcdef") $ drop 1 v) == 6 
        "ecl" -> 
          any (== v) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        "pid" ->
          on_num (\_ -> length v == 9) v
        other -> 
          trace ("other k: " <> show other) False
    on_num f v = 
      case maybe_read v of
        Nothing -> False
        Just n -> f n

maybe_read = fmap fst . listToMaybe . reads

parse :: [String] -> [(String, String)]
parse s = 
  map parse_kv $ filter (/= "") s
 where 
  parse_kv kv = 
   case splitOn ":" kv of
    [a, b] -> (a, b)
    other  -> error $ "invalid entry: " <> kv
