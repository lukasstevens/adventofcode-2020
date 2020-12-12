module Main where

import Data.List
import Data.Maybe
import Debug.Trace

data Direction = N | E | S | W 
  deriving (Show, Eq, Enum)

nextDir W = N
nextDir d = succ d

prevDir N = W
prevDir d = pred d

direction d = [(0, 1), (1, 0), (0, -1), (-1, 0)] !! fromEnum d

moveDir (i, j) dir u = (i + h * u, j + v * u)
  where
    (h, v) = direction dir

charToDir 'N' = N
charToDir 'E' = E
charToDir 'S' = S
charToDir 'W' = W

rotate90 (i, j) = (-j, i)

move :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
move ((i, j), (wi, wj)) ('F', u) = ((i + wi * u, j + wj * u), (wi, wj))
move (pos, wp) ('L', u) = (pos, iterate rotate90 wp !! div u 90)
move (pos, wp) ('R', u) = (pos, iterate rotate90 wp !! (3 * div u 90))
move (pos, wp) (wpDir, u) = (pos, moveDir wp (charToDir wpDir) u)

main :: IO ()
main = do
  moves <- map (\s -> (head s, read (tail s))) . lines <$> readFile "input.txt"
  print $ (\((i, j),_) -> abs i + abs j) $ foldl' move ((0, 0), (10, 1)) moves
