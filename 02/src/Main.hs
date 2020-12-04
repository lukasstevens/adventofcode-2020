module Main where

import Data.List.Split

main :: IO ()
main = do
  pwLines <- lines <$> readFile "input.txt"
  print $ sum [1 | [m1, m2, c', _, pw] <- splitOneOf ":- " <$> pwLines,
                   let c1 = pw !! (read m1 - 1), let c2 = pw !! (read m2 - 1), let c = head c',
                   c1 == c && c2 /= c || c1 /= c && c2 == c] 
