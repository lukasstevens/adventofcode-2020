{-# LANGUAGE TypeApplications #-}
module Part1 where

import Data.List
import Data.Sequence as Seq
import Data.Function
import Data.List.Split
import System.Environment

simulate :: Seq Int -> Seq Int -> Seq Int
simulate Empty deck2 = deck2
simulate deck1 Empty = deck1
simulate (c1 :<| deck1) (c2 :<| deck2)
  | c1 > c2 = simulate (deck1 |> c1 |> c2) deck2 
  | otherwise = simulate deck1 (deck2 |> c2 |> c1)

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  [deck1, deck2] <- map (fromList . map (read @Int) . tail) . splitOn [""] . lines <$> readFile inputFile
  let winnerDeck = simulate deck1 deck2
  print $ snd $ foldl' (\(i, acc) x -> (i - 1, acc + i * x)) (Seq.length winnerDeck, 0) winnerDeck
