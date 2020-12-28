module Part1 where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import System.Environment

mapAllgs :: [([String], [String])] -> Map.Map String (Set.Set String)
mapAllgs = mapAllgs' Map.empty
  where
    mapAllgs' m [] = m
    mapAllgs' m ((is, as) : ms) = mapAllgs' m' ms
      where
        m' = foldl' (\acc a -> Map.insertWith Set.intersection a (Set.fromList is) acc) m as 

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  menu <- map ((\[is, as] -> (splitOn " " is, splitOn ", " $ init as)) . splitOn " (contains ") . lines <$> readFile inputFile
  let allgMap = mapAllgs menu
  print $ length $ filter (`Set.notMember` Set.unions (Map.elems allgMap)) $ concatMap fst menu
