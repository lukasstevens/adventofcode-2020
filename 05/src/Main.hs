module Main where

import Data.List

search :: Char -> Char -> String -> Int -> Int -> Int
search l h = search'
  where
    search' [] mi _ = mi
    search' (x : xs) mi ma 
      | x == l = search' xs mi mid 
      | x == h = search' xs (mid + 1) ma 
      | otherwise = undefined
      where
        mid = mi + ((ma - mi) `div` 2)

findSeat :: [Int] -> Int
findSeat (x : y : xs)
  | x + 2 == y = x + 1 
  | otherwise = findSeat (y : xs)
findSeat _ = undefined

main :: IO ()
main = do
  input <- map (splitAt 7) . lines <$> readFile "input.txt"
  let seats = sort [search 'F' 'B' r 0 127 * 8 + search 'L' 'R' c 0 7  | (r, c) <- input]
  print $ findSeat seats

