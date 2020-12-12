module Part1 where

import Data.List
import Data.Maybe
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  numbers <- sort . map read . lines <$> readFile inputFile :: IO [Integer]
  let diffs = zipWith (-) (numbers ++ [last numbers + 3]) (0 : numbers)
  print $ length (filter (== 1) diffs) * length (filter (== 3) diffs)
