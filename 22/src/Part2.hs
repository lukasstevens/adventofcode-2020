{-# LANGUAGE TypeApplications #-}
module Part2 where

import Data.List
import qualified Data.Set as Set 
import Data.Sequence as Seq
import Data.List.Split
import System.Environment

simulate :: Set.Set (Seq Int, Seq Int) -> Seq Int -> Seq Int
         -> Either (Seq Int) (Seq Int) 
simulate _ Empty deck2 = Right deck2
simulate _ deck1 Empty = Left deck1
simulate configs deck1@(c1 :<| cs1) deck2@(c2 :<| cs2)
  | (deck1, deck2) `Set.member` configs = Left deck1
  | c1 <= Seq.length cs1 && c2 <= Seq.length cs2 =
      case simulate configs' (Seq.take c1 cs1) (Seq.take c2 cs2) of
        Left _ -> simulate configs' (cs1 |> c1 |> c2) cs2
        Right _ -> simulate configs' cs1 (cs2 |> c2 |> c1)
  | c1 > c2 = simulate configs' (cs1 |> c1 |> c2) cs2 
  | otherwise = simulate configs' cs1 (cs2 |> c2 |> c1)
  where
    configs' = Set.insert (deck1, deck2) configs

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  [deck1, deck2] <- map (fromList . map (read @Int) . tail) . splitOn [""] . lines <$> readFile inputFile
  let winnerDeck = fromEither $ simulate Set.empty deck1 deck2
  print $ snd $ foldl' (\(i, acc) x -> (i - 1, acc + i * x)) (Seq.length winnerDeck, 0) winnerDeck
