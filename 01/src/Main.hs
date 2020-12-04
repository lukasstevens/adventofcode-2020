module Main where

import Data.List

main :: IO ()
main = do
  numbers <- map read . lines <$> readFile "input.txt" :: IO [Int]
  print $ head [x * y * z | x <- numbers, y <- numbers, z <- numbers, x + y + z == 2020] 
