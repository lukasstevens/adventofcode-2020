module Part1 where

import Data.List
import Data.List.Split
import Data.Ix
import Text.Read
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile 
  let passports = map unwords $ splitWhen null $ lines input
  let possFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
  print $ sum [1 | fields <- splitOneOf " \n" <$> passports,
                   let kvs = splitOn ":" <$> fields,
                   sort (["cid"] `union` map head kvs) == possFields]
