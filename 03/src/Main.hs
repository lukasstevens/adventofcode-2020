module Main where

countTrees :: (Int, Int) -> [String] -> Int
countTrees (r, d) = countTrees'
  where
    countTrees' [] = 0
    countTrees' (xs : xss) = fromEnum (head xs == '#') + countTrees' (drop (d - 1) $ drop r <$> xss)

main :: IO ()
main = do
  forest <- map cycle . lines <$> readFile "input.txt"
  print $ product $ flip countTrees forest <$> (1,2):zip [1,3,5,7] (repeat 1)
