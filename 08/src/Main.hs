module Main where

import Data.List
import Data.List.Split
import Data.Maybe

run :: [Int] -> [[String]] -> Int -> Int -> Maybe Int
run exec instr pc acc
  | execCount > 0 = Nothing 
  | pc >= length instr = Just acc
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
  instrs <- map (splitOn " " . filter (/= '+')) . lines <$> readFile "input.txt"
  print $ head $ catMaybes [ run (repeat 0) (setAt instrs i ["nop", "0"]) 0 0 | i <- [0..length instrs - 1],
                             let (instr : _) = instrs !! i, instr == "jmp" ]
