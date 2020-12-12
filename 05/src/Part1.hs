module Part1 where

import Data.List
import System.Environment

fromBinary :: [Bool] -> Int
fromBinary = foldl' (\acc x -> 2 * acc + fromEnum x) 0

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- map (splitAt 7) . lines <$> readFile inputFile 
  print $ maximum [fromBinary (map (== 'B') r) * 8 + fromBinary (map (== 'R') c)  | (r, c) <- input]

