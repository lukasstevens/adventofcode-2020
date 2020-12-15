module Part2 where

import qualified Data.IntMap.Strict as M
import qualified Data.Sequence as S
import System.Environment

turns :: M.IntMap Int -> S.Seq Int -> [Int] 
turns m nums@(_ S.:|> prev) =
  case M.lookup prev m of
    Nothing -> 0 : turns m' (nums S.|> 0)
    Just i ->
      let idxDiff = S.length nums - i
       in idxDiff : turns m' (nums S.|> idxDiff) 
  where
    m' = M.insert prev (S.length nums) m
                                            
main :: IO ()
main = do
  inputFile <- head <$> getArgs
  numbers <- map read . words <$> readFile inputFile
  let ts = turns (M.fromList $ init $ zip numbers [1..]) $ S.fromList numbers
  print $ ts !! (30000000 - length numbers - 1)
  
