module Part2 where

import Data.List
import Data.Maybe
import System.Environment

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (x : _) 0 = Just x
atMay (x : xs) n = atMay xs (n - 1)

atMay2 :: [[a]] -> (Int, Int) -> Maybe a 
atMay2 xss (r, c) = do
  row <- atMay xss r
  atMay row c

fixpoint f x = if f x == x then x else fixpoint f (f x)

applySeatingRules :: [String] -> [String]
applySeatingRules seating = [ [ applySeatRule i j | j <- [0..length (seating !! i) - 1] ] | i <- [0..length seating - 1] ]
  where
    findSeat (h, v) i j = do
      seat <- atMay2 seating (i + h, j + v)
      if seat == '.'
        then findSeat (h, v) (i + h) (j + v)
        else return seat

    surrSeats i j = [ findSeat (h, v) i j | v <- [-1..1], h <- [-1..1], v /= 0 || h /= 0 ]

    applySeatRule i j
      | seat == 'L' && '#' `notElem` catMaybes (surrSeats i j) = '#'
      | seat == '#' && length (filter (== '#') $ catMaybes $ surrSeats i j) >= 5 = 'L'
      | otherwise = seat
      where
        seat = seating !! i !! j

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  seating <- lines <$> readFile inputFile 
  print $ length $ filter (== '#') $ concat $ fixpoint applySeatingRules seating
