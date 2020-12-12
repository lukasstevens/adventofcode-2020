module Part2 where

import Data.List.Split
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  pwLines <- lines <$> readFile inputFile 
  print $ sum [1 | [m1, m2, c', _, pw] <- splitOneOf ":- " <$> pwLines,
                   let c1 = pw !! (read m1 - 1), let c2 = pw !! (read m2 - 1), let c = head c',
                   c1 == c && c2 /= c || c1 /= c && c2 == c] 
