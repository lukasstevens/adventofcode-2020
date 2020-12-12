module Part2 where

import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs 
  answers <- splitWhen null . map nub . lines <$> readFile inputFile 
  print $ sum $ length . foldl intersect ['a'..'z'] <$> answers

