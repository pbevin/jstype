module Compiler (module Compiler, module CompiledExpr) where

import CompiledExpr
import Runtime.Types
import Expr

compile :: Expr -> CompiledExpr
compile expr = case expr of
  Num n            -> OpConst (VNum n)
  Str s            -> OpConst (VStr s)
  Boolean b        -> OpConst (VBool b)
  LiteralNull      -> OpConst (VNull)
  LiteralUndefined -> OpConst (VUndef)
  ReadVar v        -> OpVar v
  This             -> OpThis
  MemberDot e x    -> compilePropAccDot e x
  MemberGet e x    -> compilePropAccBracket e x
  BinOp "&&" e1 e2 -> compileShortCircuitAnd e1 e2
  BinOp "||" e1 e2 -> compileShortCircuitOr e1 e2
  _                -> Interpreted expr


compilePropAccDot :: Expr -> Ident -> CompiledExpr
compilePropAccDot expr ident = BasicBlock [ compile expr, OpToValue, OpGet ident ]

compilePropAccBracket :: Expr -> Expr -> CompiledExpr
compilePropAccBracket expr ix = BasicBlock [ compile ix, OpToValue, compile expr, OpToValue, OpGet2 ]

compileShortCircuitAnd :: Expr -> Expr -> CompiledExpr
compileShortCircuitAnd e1 e2 =
  let compe1 = [ compile e1, OpToValue, OpDup, OpToBoolean ]
      compe2 = [ OpDiscard, compile e2, OpToValue ]
  in BasicBlock $ compe1 ++ [ IfEq (VBool True) (BasicBlock compe2) ]

compileShortCircuitOr :: Expr -> Expr -> CompiledExpr
compileShortCircuitOr e1 e2 =
  let compe1 = [ compile e1, OpToValue, OpDup, OpToBoolean]
      compe2 = [ OpDiscard, compile e2, OpToValue ]
  in BasicBlock $ compe1 ++ [ IfEq (VBool False) (BasicBlock compe2) ]
