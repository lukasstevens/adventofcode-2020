module Part1 where

import Data.List
import qualified Data.Text as Text
import qualified Data.IntMap as M
import Data.List.Split
import System.Environment
import Text.Regex.TDFA

parseRules :: M.IntMap (Either String [[Int]]) -> [String] -> M.IntMap (Either String [[Int]])
parseRules m [] = m
parseRules m (r : rs) = parseRules (parseRule m r) rs
  where
    parseRule m r = 
      let
        [ri, rhs] = splitOn ": " r
        alts = if '"' `elem` rhs
                  then Left $ filter (/= '"') rhs
                  else Right $ map read . words <$> splitOn " | " rhs
      in
        M.insert (read ri) alts m


regexFromRules :: M.IntMap (Either String [[Int]]) -> Int -> String
regexFromRules m r = case m M.! r of
                       Left s -> s
                       Right rs -> (\s -> "(" ++ s ++ ")") . intercalate "|" $ concatMap (regexFromRules m) <$> rs
main :: IO ()
main = do
  inputFile <- head <$> getArgs
  [rules, ws] <- splitOn [""] . lines <$> readFile inputFile
  let rm = parseRules M.empty rules
  let regex = makeRegex $ "\\`" ++ regexFromRules rm 0 ++ "\\'" :: Regex
  print $ foldl' (\acc s -> acc + fromEnum (match regex s :: Bool)) 0 $ map Text.pack ws
