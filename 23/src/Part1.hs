{-# LANGUAGE TypeApplications #-}
module Part1 where

import Data.Ord
import Data.Char
import Data.List
import Data.List.Split
import System.Environment

move (cc : c1 : c2 : c3 : cs) = res : move res
  where
    dc = head $ sortOn Down (filter (< cc) cs) ++ [maximum cs]
    (ts, _:hs) = break (== dc) cs 
    res = ts ++ [dc, c1, c2, c3] ++ hs ++ [cc]

after1 cs = hs ++ ts
  where
    (ts, _:hs) = break (== 1) cs

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  cups <- map (read @Int . pure) . filter (not . isSpace) <$> readFile inputFile
  print $ after1 $ move cups !! 99
