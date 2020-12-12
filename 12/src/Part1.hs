module Part1 where

import Data.List
import Data.Maybe
import System.Environment

data Direction = N | E | S | W 
  deriving (Show, Eq, Enum, Read)

nextDir W = N
nextDir d = succ d

moveDir :: (Int, Int) -> Direction -> Int -> (Int, Int)
moveDir (i, j) dir u = (i + h * u, j + v * u)
  where
    (h, v) = [(0, 1), (1, 0), (0, -1), (-1, 0)] !! fromEnum dir

move :: ((Int, Int), Direction) -> (Char, Int) -> ((Int, Int), Direction)
move (pos, dir) ('F', u) = (moveDir pos dir u, dir)
move (pos, dir) ('L', u) = (pos, iterate (nextDir . nextDir . nextDir) dir !! div u 90)
move (pos, dir) ('R', u) = (pos, iterate nextDir dir !! div u 90)
move (pos, dir) (mDir, u) = (moveDir pos (read [mDir]) u, dir)

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  moves <- map (\s -> (head s, read (tail s))) . lines <$> readFile inputFile 
  print $ (\((i, j),_) -> abs i + abs j) $ foldl' move ((0, 0), E) moves
