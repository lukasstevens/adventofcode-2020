module Part1 where

import Data.List
import Data.Ix
import qualified Data.Set as Set
import System.Environment

surrPoss (x, y, z) = Set.fromList [(x + a, y + b, z + c) | a <- [-1..1], b <- [-1..1], c <- [-1..1],
                                                           a /= 0 || b /= 0 || c /= 0]
simulate poss =
  let
    allPoss = Set.foldl' (\s p -> s `Set.union` surrPoss p) poss poss
    becomesActive p
      | p `Set.member` poss = inRange (2, 3) $ Set.size $ surrPoss p `Set.intersection` poss
      | otherwise = Set.size (surrPoss p `Set.intersection` poss) == 3
    newPoss = Set.filter becomesActive allPoss 
   in 
    newPoss : simulate newPoss
    

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- map (map (== '#')) . lines <$> readFile inputFile
  let s = Set.fromList [ (x, y, 0) | x <- [0..length input - 1], y <- [0..length (input !! x) - 1],
                                   input !! x !! y ]
  print $ Set.size $ simulate s !! 5
