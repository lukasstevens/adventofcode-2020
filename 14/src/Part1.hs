module Part1 where

import qualified Data.Map as M
import Data.List.Split
import System.Environment

assigns :: String -> M.Map Int String -> [[String]] -> M.Map Int String 
assigns _ mem [] = mem 
assigns _ mem (["mask", ma] : as) = assigns ma mem as
assigns ma mem (['m':'e':'m':'[':i, val] : as) = assigns ma mem' as
  where
    ix = read $ init i

    mask 'X' x = x
    mask y _ = y 
    mem' = M.insert ix (zipWith mask ma $ toBinary $ read val) mem

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
