module Main where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  answers <- splitWhen null . map nub . lines <$> readFile "input.txt"
  print $ sum $ length . foldl intersect ['a'..'z'] <$> answers

