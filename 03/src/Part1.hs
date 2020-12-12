module Part1 where

import System.Environment

countTrees :: (Int, Int) -> [String] -> Int
countTrees (r, d) = countTrees'
  where
    countTrees' [] = 0
    countTrees' (xs : xss) = fromEnum (head xs == '#') + countTrees' (drop (d - 1) $ drop r <$> xss)

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  forest <- map cycle . lines <$> readFile inputFile 
  print $ countTrees (3, 1) forest
