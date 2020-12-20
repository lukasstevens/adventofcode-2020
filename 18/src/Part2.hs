{-# LANGUAGE FlexibleContexts #-}
module Part2 where

import Data.Either
import Data.List
import Text.Parsec
import System.Environment

data Expr = Const Int | Mul Expr Expr | Add Expr Expr
  deriving Show

parseConst :: Stream s m Char => ParsecT s u m Expr
parseConst = Const . read <$> many1 digit 

parseAdds :: Stream s m Char => ParsecT s u m Expr 
parseAdds = chainr1 (parseMulsParen <|> parseConst) (char '+' *> pure Add) 

parseMuls :: Stream s m Char => ParsecT s u m Expr
parseMuls = chainr parseAdds (char '*' *> pure Mul) (Const 1)

parseMulsParen :: Stream s m Char => ParsecT s u m Expr
parseMulsParen = between (char '(') (char ')') parseMuls

eval :: Expr -> Int
eval (Mul e1 e2) = eval e1 * eval e2
eval (Add e1 e2) = eval e1 + eval e2
eval (Const x) = x

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- lines <$> readFile inputFile
  print $ sum $ eval . fromRight (Const 0) . parse parseMuls "" . filter (/= ' ') <$> input
