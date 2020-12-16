module Part2 where

import Data.Bifunctor (bimap, second)
import Data.List
import Data.List.Split
import Data.Ix
import System.Environment

toSndRange :: [String] -> (String, [(Int, Int)])
toSndRange [field, rs] =
  case wordsBy (`elem` "- ") rs of
    [f1, t1, _, f2, t2] -> (field, bimap read read <$> [(f1, t1), (f2, t2)])

determineFields :: [(Int, [String])] -> [(Int, String)]
determineFields [] = []
determineFields fss = (second head <$> fss1) ++
                      determineFields (second (\\ concatMap snd fss1) <$> fss')
  where
    (fss1, fss') = partition ((== 1) . length . snd) fss

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- splitOn [""] . lines <$> readFile inputFile

  let fields = map (toSndRange . splitOn ": ") $ head input
  let myTicket = map read . splitOn "," $ input !! 1 !! 1
  let isValid t = and [ any (`inRange` n) (concatMap snd fields) | n <- t ]
  let nearbyTickets = filter isValid . map (map read . splitOn ",") $ tail $ input !! 2

  let possFieldsVal n = [ f | (f, rs) <- fields, any (`inRange` n) rs ]
  let possFieldsTickets = map possFieldsVal <$> nearbyTickets
  let possFields = foldr1 intersect <$> transpose possFieldsTickets
  let detFields = map snd $ sortOn fst $ determineFields $ zip [0..] possFields

  print $ product [ v::Integer | (f, v) <- zip detFields myTicket, "departure" `isPrefixOf` f ]
