module Part1 where

import Data.List
import System.Environment


turns nums = if prev `elem` init nums
                then lastIndicesDiff : turns (nums ++ [lastIndicesDiff])
                else 0 : turns (nums ++ [0]) 
  where
    prev = last nums
    indices = elemIndices prev nums
    lastIndicesDiff = last indices - last (init indices)
                                            
main :: IO ()
main = do
  inputFile <- head <$> getArgs
  numbers <- map read . words <$> readFile inputFile
  print $ turns numbers !! (2020 - length numbers - 1)
  
