module Part1 where

import qualified Data.Set as Set
import Data.List
import System.Environment

coords :: (Int, Int) -> String -> (Int, Int)
coords (x, y) ('s':'e':ds) = coords (x, y - 1) ds
coords (x, y) ('s':'w':ds) = coords (x - 1, y - 1) ds
coords (x, y) ('n':'e':ds) = coords (x + 1, y + 1) ds
coords (x, y) ('n':'w':ds) = coords (x, y + 1) ds
coords (x, y) ('w':ds) = coords (x - 1, y) ds
coords (x, y) ('e':ds) = coords (x + 1, y) ds
coords (x, y) _ = (x, y)

flipTiles = flipTiles' Set.empty
  where
    flipTiles' s [] = s
    flipTiles' s (ds : dss)
      | loc `Set.member` s = flipTiles' (loc `Set.delete` s) dss
      | otherwise = flipTiles' (loc `Set.insert` s) dss
      where
        loc = coords (0, 0) ds

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  dirs <- lines <$> readFile inputFile
  print $ Set.size $ flipTiles dirs 
