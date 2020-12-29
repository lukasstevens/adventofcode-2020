{-# LANGUAGE TupleSections #-}
module Part2 where

import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Function
import Data.List.Split
import System.Environment

type Tile = [[Bool]]

orientations :: [[a]] -> [[[a]]] 
orientations xss = [map r . c . t $ xss | r <- [id, reverse], c <- [id, reverse], t <- [id, transpose]]

neighbors :: [Tile] -> Tile -> [((Int, Int), Tile)]
neighbors tiles t = catMaybes
  [ ((-1, 0),) <$> find (\t' -> head t == last t') tiles
  , ((1, 0),) <$> find (\t' -> last t == head t') tiles
  , ((0, -1),) <$> find (\t' -> head (transpose t) == last (transpose t')) tiles
  , ((0, 1),) <$> find (\t' -> last (transpose t) == head (transpose t')) tiles ]

coordTiles :: [Tile] -> [((Int, Int), Tile)]
coordTiles tiles = flip evalState [] $ coordTiles' (0, 0) $ head tiles
  where
    allTiles = concatMap orientations tiles

    coordTiles' :: (Int, Int) -> Tile
                -> State [(Int, Int)] [((Int, Int), Tile)]
    coordTiles' (x, y) t = do
      vis <- get
      if (x, y) `elem` vis
         then return []
         else do
           vis <- put $ (x, y) : vis
           let nbs = neighbors (filter (`notElem` orientations t) allTiles) t
           otherTiles <- sequence [coordTiles' (x + i, y + j) t' | ((i, j), t') <- nbs]
           return $ ((x, y), t) : concat otherTiles

flatten xss
  | all null xss = []
  | otherwise = concatMap head xss : flatten (map tail xss)

rmBorders = map rmFirstLast . rmFirstLast
  where
    rmFirstLast = tail . init

seaMonster = map (== '#') <$>
  [ "                  # " 
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   " ] 

monsterH = length seaMonster
monsterW = length $ head seaMonster
monsterPos = [ (x, y) | x <- [0..monsterH - 1], y <- [0..monsterW - 1], seaMonster !! x !! y ]

matchMonster ps (i, j) tiles
  | length window < monsterH = nub ps
  | length (head window) < monsterW = matchMonster ps (i + 1, 0) tiles
  | hasMonster = matchMonster (monsterPos' ++ ps) (i, j + 1) tiles
  | otherwise = matchMonster ps (i, j + 1) tiles
  where
    monsterPos' = map (\(x, y) -> (x + i, y + j)) monsterPos 
    hasMonster = all (\(m, w) -> not m || w) $ on zip concat seaMonster window
    window = map (take monsterW) $ take monsterH 
              $ map (drop j) $ drop i tiles

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  tiles <- map (map (map (== '#')) . tail) . splitOn [""] . lines <$> readFile inputFile
  let tileCoords = sortOn fst $ coordTiles tiles
  let chunkSize = 1 + maximum (map (snd . fst) tileCoords) - minimum (map (snd . fst) tileCoords)
  let tiled = concatMap flatten $ chunksOf chunkSize $ rmBorders . snd <$> tileCoords
  let monsterMatches = maximum $ map (length . matchMonster [] (0, 0)) $ orientations tiled
  print $ sum (map (sum . map fromEnum) tiled) - monsterMatches
