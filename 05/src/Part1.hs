module Part1 where

import Data.List
import System.Environment

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

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- map (splitAt 7) . lines <$> readFile inputFile 
  print $ maximum [search 'F' 'B' r 0 127 * 8 + search 'L' 'R' c 0 7  | (r, c) <- input]

