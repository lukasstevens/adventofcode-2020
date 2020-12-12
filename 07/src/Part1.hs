module Part1 where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Environment

buildGraph :: M.Map String [(Integer, String)] -> [(String, [String])] -> M.Map String [(Integer, String)]
buildGraph m [] = m
buildGraph m ((from, tos) : xs) = buildGraph (M.insertWith (++) from tos' m) xs
  where
    tos' = (\(c, b) -> (read c :: Integer, tail b)) . span (/= ' ') <$> tos

hasGold :: M.Map String [(Integer, String)] -> String -> Bool
hasGold m b = "shiny gold" `elem` bags || any (hasGold m) bags
  where
    bags = snd <$> m M.! b

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  bagDescriptions <- lines <$> readFile inputFile 
  let bags = (\[from, to] -> (dropLastWord from, filter (/= "no other") $ dropLastWord <$> splitOn ", " (init to))) . splitOn " contain " <$> bagDescriptions
  let graph = buildGraph M.empty bags
  print $ sum [ 1 | b <- fst <$> bags, hasGold graph b ]
  where
    dropLastWord = unwords . init . words


