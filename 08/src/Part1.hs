module Part1 where

import Data.List
import Data.List.Split
import Data.Maybe
import System.Environment

run :: [Int] -> [[String]] -> Int -> Int -> Int
run exec instr pc acc
  | execCount > 0 = acc 
  | otherwise = case instr !! pc of
                  ["nop", _] -> run exec' instr (pc + 1) acc 
                  ["acc", arg] -> run exec' instr (pc + 1) $ acc + read arg 
                  ["jmp", arg] -> run exec' instr (pc + read arg) acc
  where
    execCount = exec !! pc
    exec' = setAt exec pc $ execCount + 1

setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  instrs <- map (splitOn " " . filter (/= '+')) . lines <$> readFile inputFile
  print $ run (repeat 0) instrs 0 0
