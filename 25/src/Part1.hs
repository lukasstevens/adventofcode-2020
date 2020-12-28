module Part1 where

import Data.Maybe
import Data.List
import System.Environment

loops :: Int -> [Int]
loops subj = iterate ((`mod` 20201227) . (* subj)) 1

loopSize :: Int -> Int
loopSize n = fromJust $ elemIndex n $ loops 7

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  [cardPub, doorPub] <- map read . lines <$> readFile inputFile
  print $ loops doorPub !! loopSize cardPub
