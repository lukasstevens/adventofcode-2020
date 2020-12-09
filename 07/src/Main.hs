module Main where

import Debug.Trace
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as M

buildGraph :: M.Map String [(Integer, String)] -> [(String, [String])] -> M.Map String [(Integer, String)]
buildGraph m [] = m
buildGraph m ((from, tos) : xs) = buildGraph (M.insertWith (++) from tos' m) xs
  where
    tos' = (\(c, b) -> (read c :: Integer, tail b)) . span (/= ' ') <$> tos

hasGold :: M.Map String [(Integer, String)] -> String -> Bool
hasGold m b = "shiny gold" `elem` bags || any (hasGold m) bags
  where
    bags = snd <$> m M.! b

bagSize :: M.Map String [(Integer, String)] -> String -> Integer
bagSize m b = 1 + sum [ i * bagSize m cb | (i, cb) <- m M.! b ]

main :: IO ()
main = do
  bagDescriptions <- lines <$> readFile "input.txt"
  let bags = (\[from, to] -> (dropLastWord from, filter (/= "no other") $ dropLastWord <$> splitOn ", " (init to))) . splitOn " contain " <$> bagDescriptions
  let graph = buildGraph M.empty bags
  print $ sum [ 1 | b <- fst <$> bags, hasGold graph b ]
  print $ bagSize graph "shiny gold" - 1
  where
    dropLastWord = unwords . init . words


