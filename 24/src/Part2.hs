module Part2 where

import qualified Data.Set as Set
import Data.Foldable
import Data.List
import System.Environment

neighbors :: (Int, Int) -> Set.Set (Int, Int)
neighbors (x, y) = Set.fromList 
  [ (x, y - 1), (x - 1, y - 1), (x + 1, y + 1)
  , (x, y + 1), (x - 1, y), (x + 1, y)]

coords :: (Int, Int) -> String -> (Int, Int)
coords (x, y) ('s':'e':ds) = coords (x, y - 1) ds
coords (x, y) ('s':'w':ds) = coords (x - 1, y - 1) ds
coords (x, y) ('n':'e':ds) = coords (x + 1, y + 1) ds
coords (x, y) ('n':'w':ds) = coords (x, y + 1) ds
coords (x, y) ('w':ds) = coords (x - 1, y) ds
coords (x, y) ('e':ds) = coords (x + 1, y) ds
coords (x, y) _ = (x, y)

initTiles = initTiles' Set.empty
  where
    initTiles' s [] = s
    initTiles' s (ds : dss)
      | loc `Set.member` s = initTiles' (loc `Set.delete` s) dss
      | otherwise = initTiles' (loc `Set.insert` s) dss
      where
        loc = coords (0, 0) ds

flipTiles 0 tiles = tiles
flipTiles n tiles = flipTiles (n - 1) $ stayBlacks `Set.union` becomeBlacks 
  where
    rule1 x =
      let nc = Set.size $ neighbors x `Set.intersection` tiles
       in if nc == 0 || nc > 2 then Set.empty else Set.singleton x

    rule2 x =
      let nc = Set.size $ neighbors x `Set.intersection` tiles
       in if nc == 2 then Set.singleton x else Set.empty
    
    stayBlacks = Set.foldr (\x acc -> rule1 x `Set.union` acc) Set.empty tiles
    blackNeighbors = Set.foldr (\x acc -> neighbors x `Set.union` acc) Set.empty tiles
    becomeBlacks = Set.foldr (\x acc -> rule2 x `Set.union` acc) Set.empty blackNeighbors

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  dirs <- lines <$> readFile inputFile
  print $ Set.size $ flipTiles 100 $ initTiles dirs 
