module Part2 where

import Data.List
import Data.Maybe
import System.Environment

findNonSum n xs
  | xs !! n `elem` [ xs !! i + xs !! j | i <- [0..n - 1], j <- [0..n - 1], i /= j ] = findNonSum n $ tail xs 
  | otherwise = xs !! n

findCont n xs 
  | sum tryCont == n = minimum tryCont + maximum tryCont 
  | otherwise = findCont n $ tail xs
  where
    tryCont = foldl' (\acc x -> if sum (x : acc) <= n then x : acc else acc) [] xs 

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  numbers <- map read . lines <$> readFile inputFile :: IO [Int]
  let magic = findNonSum 25 numbers 
  print $ findCont magic numbers
