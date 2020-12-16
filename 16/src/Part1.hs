module Part1 where

import Data.List
import Data.List.Split
import Data.Ix
import System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- splitOn [""] . lines <$> readFile inputFile
  let extractRanges [f1, t1, _, f2, t2] =
        [(read f1, read t1), (read f2, read t2)] :: [(Int, Int)]
  let ranges = concatMap (extractRanges . wordsBy (`elem` "- ") . last . splitOn ": ") $ head input
  let nearbyTickets = map (map read . splitOn ",") $ tail $ input !! 2
  print $ sum [n | n <- concat nearbyTickets, not (any (`inRange` n) ranges)]
