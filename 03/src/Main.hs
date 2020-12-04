module Main where

countTrees :: Int -> Int -> [String] -> Int
countTrees r d = countTrees'
  where
    countTrees' [] = 0
    countTrees' (xs : xss) = fromEnum (head xs == '#') + countTrees' (drop (d - 1) $ drop r <$> xss)

main :: IO ()
main = do
  forest <- map cycle . lines <$> readFile "input.txt"
  print $ product [countTrees r d forest | (r, d) <- [(1,1), (3,1), (5,1), (7,1), (1,2)]]
