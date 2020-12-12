module Part2 where

import Data.List
import System.Environment

findSeat :: [Int] -> Int
findSeat (x : y : xs)
  | x + 2 == y = x + 1 
  | otherwise = findSeat (y : xs)
findSeat _ = undefined

fromBinary :: [Bool] -> Int
fromBinary = foldl' (\acc x -> 2 * acc + fromEnum x) 0

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- map (splitAt 7) . lines <$> readFile inputFile 
  let seats = sort [fromBinary (map (== 'B') r) * 8 + fromBinary (map (== 'R') c)  | (r, c) <- input]
  print $ findSeat seats

