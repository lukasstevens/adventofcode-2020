module Part1 where

import Data.List
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  numbers <- map read . lines <$> readFile inputFile :: IO [Int]
  print $ head [x * y | x <- numbers, y <- numbers, x /= y, x + y == 2020] 
