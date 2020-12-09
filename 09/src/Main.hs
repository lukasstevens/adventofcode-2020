module Main where

import Data.List
import Data.Maybe

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
  numbers <- map read . lines <$> readFile "input.txt" :: IO [Int]
  let magic = findNonSum 25 numbers 
  print magic
  print $ findCont magic numbers
