module Part1 where

import Data.Ord
import Data.List
import Data.List.Split
import System.Environment


main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- lines <$> readFile inputFile
  let time = read (head input)
  let buses = map read $ filter (/= "x") $ splitOn "," (input !! 1) :: [Int]
  print $ uncurry (*) $ minimumBy (comparing snd) [ (b, b - mod time b) | b <- buses ] 
