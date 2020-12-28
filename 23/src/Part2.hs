{-# LANGUAGE TypeApplications #-}
module Part2 where

import Debug.Trace
import Control.Applicative
import Data.Foldable
import Data.Maybe
import qualified Data.IntMap as Map
import Data.Char
import Data.List
import System.Environment

move 0 m _ = m
move n m cc = move (n - 1) m' (m' Map.! cc)
  where
    [c1, c2, c3] = take 3 $ tail $ iterate (m Map.!) cc

    Just (max, _) = Map.lookupMax m
    Just dc = lookupLT cc <|> lookupLT (max + 1)

    lookupLT k = do
      (l, _) <- Map.lookupLT k m
      if l `elem` [cc, c1, c2, c3]
         then lookupLT l
         else return l

    m' = foldr' (uncurry Map.insert) m [(cc, m Map.! c3), (c3, m Map.! dc), (dc, c1)]

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  cups <- (++ [10..1000000]) . map (read @Int . pure) . filter (not . isSpace) <$> readFile inputFile
  let m = Map.fromList $ zip cups (tail cups ++ [head cups])
  let res = move 10000000 m $ head cups
  print $ product $ map toInteger $ take 2 $ tail $ iterate (res Map.!) 1
