module Part1 where

import Data.List
import Data.Function
import Data.List.Split
import System.Environment

fromBools :: [Bool] -> Int
fromBools = foldl' (\acc x -> 2 * acc + fromEnum x) 0

tileBorders :: [[Bool]] -> [Int]
tileBorders ts = fromBools <$> [r $ f l | f <- [head, last], r <- [id, reverse], l <- [ts, transpose ts]]

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  tiles <- map (\ts -> (head ts, map (== '#') <$> tail ts)) . splitOn [""] . lines <$> readFile inputFile
  let borders = concatMap (\(t, ts) -> zip (repeat t) $ tileBorders ts) tiles
  let borderMatching = filter ((== 2) . length) $ groupBy ((==) `on` snd) $ sortOn snd borders
  let borderTiles = [t | (t, _) <- tiles, sum (map (fromEnum . (t `elem`) . map fst) borderMatching) == 4]
  print $ product $ read . drop 5 . init <$> borderTiles
