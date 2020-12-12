module Part1 where

import Data.List
import Data.Maybe
import System.Environment

findNonSum n xs
  | xs !! n `elem` [ xs !! i + xs !! j | i <- [0..n - 1], j <- [0..n - 1], i /= j ] = findNonSum n $ tail xs 
  | otherwise = xs !! n

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  numbers <- map read . lines <$> readFile inputFile :: IO [Int]
  print $ findNonSum 25 numbers 
