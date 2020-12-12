module Part2 where

import Data.List
import Data.Maybe
import System.Environment

data Direction = N | E | S | W 
  deriving (Show, Eq, Enum, Read)

moveDir :: (Int, Int) -> Direction -> Int -> (Int, Int)
moveDir (i, j) dir u = (i + h * u, j + v * u)
  where
    (h, v) = [(0, 1), (1, 0), (0, -1), (-1, 0)] !! fromEnum dir

rotate90 (i, j) = (-j, i)

move :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
move ((i, j), (wi, wj)) ('F', u) = ((i + wi * u, j + wj * u), (wi, wj))
move (pos, wp) ('L', u) = (pos, iterate rotate90 wp !! div u 90)
move (pos, wp) ('R', u) = (pos, iterate (rotate90 . rotate90 . rotate90) wp !! div u 90)
move (pos, wp) (wpDir, u) = (pos, moveDir wp (read [wpDir]) u)

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  moves <- map (\s -> (head s, read (tail s))) . lines <$> readFile inputFile 
  print $ (\((i, j),_) -> abs i + abs j) $ foldl' move ((0, 0), (10, 1)) moves
