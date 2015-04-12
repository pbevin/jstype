module Eval (jsEvalExpr) where

import Parse
import Expr

instance Num JSNum where
  (JSNum a) + (JSNum b) = JSNum (a+b)

jsEvalExpr :: String -> JSNum
jsEvalExpr str = evalExpr $ parseExpr str

evalExpr :: Expr -> JSNum
evalExpr (Num n) = n
evalExpr (BinOp op e1 e2) = evalExpr e1 + evalExpr e2
