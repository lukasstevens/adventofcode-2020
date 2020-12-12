module Part2 where

import Data.List
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  numbers <- map read . lines <$> readFile inputFile :: IO [Int]
  print $ head [x * y * z | x <- numbers, y <- numbers, z <- numbers, x + y + z == 2020] 
