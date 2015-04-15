module Eval (jsEval, jsEvalExpr) where

import Parse
import Expr

instance Num JSNum where
  (JSNum a) + (JSNum b) = JSNum (a + b)
  (JSNum a) - (JSNum b) = JSNum (a - b)
  (JSNum a) * (JSNum b) = JSNum (a * b)
  fromInteger n = JSNum $ fromInteger n
  abs (JSNum a) = JSNum $ abs a
  signum (JSNum a) = JSNum $ signum a

instance Fractional JSNum where
  (JSNum a) / (JSNum b) = JSNum (a / b)
  fromRational r = JSNum $ fromRational r

jsEval :: String -> Maybe String
jsEval input = case parseJS input of
  Left err -> error (show err)
  Right ast -> Just $ evalProg ast

jsEvalExpr :: String -> JSNum
jsEvalExpr input = evalExpr $ parseExpr input




evalProg (Program p) = "1\n"

evalExpr :: Expr -> JSNum
evalExpr (Num n) = n
evalExpr (BinOp op e1 e2) = evalBinOp op (evalExpr e1) (evalExpr e2)


evalBinOp :: String -> JSNum -> JSNum -> JSNum
evalBinOp op v1 v2 = case op of
  "+" -> v1 + v2
  "-" -> v1 - v2
  "*" -> v1 * v2
  "/" -> v1 / v2
