module Part2 where

import Data.List.Split
import System.Environment

euclidean :: Integer -> Integer -> (Integer, Integer)
euclidean a b = euclidean' (a, b) (1, 0) (0, 1)
  where
    euclidean' (_, 0) (s0, _) (t0, _) = (s0, t0)
    euclidean' (r0, r1) (s0, s1) (t0, t1) =
      euclidean' (r1, r0 - quotient * r1) (s1, s0 - quotient * s1) (t1, t0 - quotient * t1)
      where
        quotient = r0 `div` r1

solveEquations :: [(Integer, Integer)] -> (Integer, Integer)
solveEquations [e] = e
solveEquations ((a1, n1) : (a2, n2) : es) = solveEquations (e12 : es)
  where
    (m1, m2) = euclidean n1 n2 
    e12 = (a1 * m2 * n2 + a2 * m1 * n1, n1 * n2)

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- lines <$> readFile inputFile
  let buses = splitOn "," (input !! 1)
  let (a, n) = solveEquations [(mod (-i) (read b), read b) | (i, b) <- zip [0..] buses, b /= "x"]
  print $ a `mod` n 
