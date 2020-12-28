module Part2 where

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

fixAllgs :: Map.Map String (Set.Set String) -> Map.Map String String
fixAllgs m
  | Map.null m = Map.empty
  | otherwise = Map.map (head . Set.elems) fixedAllgs `Map.union` fixAllgs m' 
    where
      fixedAllgs = Map.filter ((== 1) . Set.size) m

      m' = Map.map (Set.\\ Set.unions (Map.elems fixedAllgs)) $ m Map.\\ fixedAllgs
    
main :: IO ()
main = do
  inputFile <- head <$> getArgs
  menu <- map ((\[is, as] -> (splitOn " " is, splitOn ", " $ init as)) . splitOn " (contains ") . lines <$> readFile inputFile
  let allgMap = fixAllgs $ mapAllgs menu
  putStrLn $ intercalate "," $ map snd $ Map.toAscList allgMap
