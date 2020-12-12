module Part1 where

import Data.List.Split
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  pwLines <- lines <$> readFile inputFile 
  print $ sum [1 | [m1, m2, c', _, pw] <- splitOneOf ":- " <$> pwLines,
                   let l = length $ filter (== head c') pw,
                   read m1 <= l && l <= read m2]
