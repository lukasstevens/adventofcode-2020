{-# LANGUAGE FlexibleContexts #-}
module Part1 where

import Data.Either
import Data.List
import Text.Parsec
import System.Environment

data Expr = Const Int | Bop (Int -> Int -> Int) Expr Expr | Paren Expr

parseConst :: Stream s m Char => ParsecT s u m Expr
parseConst = Const . read <$> many1 digit 

parseOperator :: Stream s m Char => ParsecT s u m (Expr -> Expr -> Expr)
parseOperator = opFromChar <$> oneOf "+*"
  where
    opFromChar '*' = Bop (*) 
    opFromChar '+' = Bop (+) 

parseExpr :: Stream s m Char => ParsecT s u m Expr
parseExpr = chainr1 (parseConst <|> parseExprParen) parseOperator
  where
    parseExprParen = Paren <$> between (char '(') (char ')') parseExpr

eval :: Expr -> Int
eval (Bop bop1 e1 (Bop bop23 e2 e3)) = eval (Bop bop23 (Const $ eval e1 `bop1` eval e2) e3)
eval (Bop bop e1 e2) = eval e1 `bop` eval e2
eval (Const x) = x
eval (Paren e) = eval e

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- lines <$> readFile inputFile
  print $ sum $ eval . fromRight (Const 0) . parse parseExpr "" . filter (/= ' ') <$> input
