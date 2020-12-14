module Part2 where

import Data.List
import qualified Data.Map.Strict as M
import Data.List.Split
import System.Environment

masks _ [] = [[]]
masks ('X' : ms) (x : xs) = map ('0':) (masks ms xs) ++ map ('1':) (masks ms xs)
masks ('1' : ms) (x : xs) = map ('1':) $ masks ms xs
masks ('0' : ms) (x : xs) = map (x:) $ masks ms xs

assigns :: String -> M.Map Int String -> [[String]] -> M.Map Int String 
assigns _ mem [] = mem 
assigns _ mem (["mask", ma] : as) = assigns ma mem as
assigns ma mem (['m':'e':'m':'[':i, val] : as) = assigns ma mem' as
  where
    adds = map fromBinary $ masks ma $ toBinary $ read $ init i 
    mem' = foldl' (\m add -> M.insert add (toBinary $ read val) m) mem adds

toBinary n = replicate (36 - length bin) '0' ++ bin 
  where
    bin = reverse $ toBinary' n
    toBinary' 0 = ""
    toBinary' n = show (n `mod` 2) ++ toBinary' (n `div` 2)

fromBinary xs = foldl (\acc x -> 2 * acc + fromEnum x) 0 $ map (== '1') xs

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- map (splitOn " = ") . lines <$> readFile inputFile
  let memory = assigns "" M.empty input 
  print $ sum $ fromBinary . snd <$> M.toList memory 
