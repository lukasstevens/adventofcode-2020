module Part2 where

import Data.List
import Data.List.Split
import Data.Ix
import Text.Read
import System.Environment

maybeRead f v = maybe False f $ readMaybe v

validate "byr" v = maybeRead (inRange (1920, 2002)) v 
validate "iyr" v = maybeRead (inRange (2010, 2020)) v 
validate "eyr" v = maybeRead (inRange (2020, 2030)) v 
validate "hgt" v
  | unit == "in" = maybeRead (inRange (59, 76)) size 
  | unit == "cm" = maybeRead (inRange (150, 193)) size 
  | otherwise = False
  where
    (size, unit) = splitAt (length v - 2) v
validate "hcl" ('#': cc) | length cc == 6 = all (`elem` (['0'..'9'] ++ ['a'..'f'])) cc
validate "hcl" _ = False
validate "ecl" v = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validate "pid" v
  | length v == 9 = all (`elem` ['0'..'9']) v
  | otherwise = False
validate "cid" _ = True
validate _ _ = undefined 

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile 
  let passports = map unwords $ splitWhen null $ lines input
  let possFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
  print $ sum [1 | fields <- splitOneOf " \n" <$> passports,
                   let kvs = splitOn ":" <$> fields,
                   sort (["cid"] `union` map head kvs) == possFields,
                   all (\[k, v] -> validate k v) kvs]
