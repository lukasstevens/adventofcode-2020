module Main where

import Data.List
import Data.Maybe


tribonacci :: Integer -> Integer
tribonacci n = tn
  where
    (tn, _, _) = foldl' (\(tn, tnp1, tnp2) _ -> (tnp1, tnp2, tn + tnp1 + tnp2)) (0, 0, 1) [0..n]

main :: IO ()
main = do
  numbers <- sort . map read . lines <$> readFile "input.txt" :: IO [Integer]
  let diffs = zipWith (-) (numbers ++ [last numbers + 3]) (0 : numbers)
  print $ product $ map (tribonacci . (+1) . genericLength) $ filter ((/= 3) . head) $ group diffs
