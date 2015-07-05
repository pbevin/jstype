module Compiler (module Compiler, module CompiledExpr) where

import Data.Maybe
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
  ArrayLiteral vs  -> compileArrayLiteral vs
  MemberDot e x    -> compilePropAccDot e x
  MemberGet e x    -> compilePropAccBracket e x
  Assign lhs "=" e -> compileAssignment lhs e
  Assign lhs op e  -> compileCompoundAssignment lhs op e
  BinOp "&&" e1 e2 -> compileShortCircuitAnd e1 e2
  BinOp "||" e1 e2 -> compileShortCircuitOr e1 e2
  BinOp "," e1 e2  -> compileSequence e1 e2
  BinOp op e1 e2   -> compileBinOp op e1 e2
  UnOp "delete" e  -> compileDelete e
  UnOp "typeof" e  -> compileTypeof e
  UnOp op e        -> compileUnary op e
  PostOp op e      -> compilePostOp op e
  Cond e1 e2 e3    -> compileCond e1 e2 e3
  FunCall f args   -> compileFunCall f args
  NewExpr f args   -> compileNewExpr f args
  RegExp r f       -> compileRegExp r f
  _                -> Interpreted expr

compileArrayLiteral :: [Maybe Expr] -> CompiledExpr
compileArrayLiteral vs =
  if all isJust vs
  then compileCompactArray (catMaybes vs)
  else compileSparseArray vs

compileCompactArray :: [Expr] -> CompiledExpr
compileCompactArray elts =
  BasicBlock $ reverse $ (OpArray $ length elts) : map compile elts

compileSparseArray :: [Maybe Expr] -> CompiledExpr
compileSparseArray elts =
  BasicBlock $ go 0 $ zip [0..] elts
    where
      go n []     = [ OpConst (vnum $ length elts), OpSparse n ]
      go n (x:xs) = case x of
        (_, Nothing) -> go n xs
        (k, Just e) -> OpConst (VNum k) : compile e : go (n+1) xs
      vnum k = VNum (fromIntegral k)

compilePropAccDot :: Expr -> Ident -> CompiledExpr
compilePropAccDot expr ident =
  BasicBlock [ compile expr, OpGetValue, OpGet ident ]

compilePropAccBracket :: Expr -> Expr -> CompiledExpr
compilePropAccBracket expr ix =
  BasicBlock [ compile expr, OpGetValue, compile ix, OpGetValue, OpGet2 ]

-- ref 11.13.1
compileAssignment :: Expr -> Expr -> CompiledExpr
compileAssignment lhs e =
  BasicBlock $ [ compile lhs, compile e, OpGetValue, OpStore ]

-- ref 11.13.2
compileCompoundAssignment :: Expr -> Ident -> Expr -> CompiledExpr
compileCompoundAssignment lhs op e =
  BasicBlock $ [ compile lhs, OpDup, OpGetValue,
                 compile e, OpGetValue,
                 OpBinary (init op), OpStore ]


compileShortCircuitAnd :: Expr -> Expr -> CompiledExpr
compileShortCircuitAnd e1 e2 =
  let compe1 = [ compile e1, OpGetValue, OpDup, OpToBoolean ]
      compe2 = [ OpDiscard, compile e2, OpGetValue ]
  in BasicBlock $ compe1 ++ [ IfTrue (BasicBlock compe2) Nop]

compileShortCircuitOr :: Expr -> Expr -> CompiledExpr
compileShortCircuitOr e1 e2 =
  let compe1 = [ compile e1, OpGetValue, OpDup, OpToBoolean]
      compe2 = [ OpDiscard, compile e2, OpGetValue ]
  in BasicBlock $ compe1 ++ [ IfTrue Nop (BasicBlock compe2) ]

compileSequence :: Expr -> Expr -> CompiledExpr
compileSequence e1 e2 =
  BasicBlock [ compile e1, OpDiscard, compile e2 ]

compileBinOp :: Ident -> Expr -> Expr -> CompiledExpr
compileBinOp op e1 e2 =
  BasicBlock [ compile e1, OpGetValue, compile e2, OpGetValue, OpBinary op ]

compileDelete :: Expr -> CompiledExpr
compileDelete e = BasicBlock [ compile e, OpDelete ]

compileTypeof :: Expr -> CompiledExpr
compileTypeof e = BasicBlock [ compile e, OpTypeof ]

compileUnary :: Ident -> Expr -> CompiledExpr
compileUnary op e =
  if op == "++" || op == "--"
  then BasicBlock [ compile e, OpDup, OpGetValue, OpModify op, OpStore ]
  else BasicBlock [ compile e, OpGetValue, OpUnary op ]

compilePostOp :: Ident -> Expr -> CompiledExpr
compilePostOp op e =
  BasicBlock [ compile e, OpDup, OpGetValue, OpToNumber, OpDup, OpRoll3, OpModify op, OpStore, OpDiscard ]

compileCond :: Expr -> Expr -> Expr -> CompiledExpr
compileCond e1 e2 e3 =
  BasicBlock [ compile e1, OpGetValue, OpToBoolean,
               IfTrue (BasicBlock [ compile e2, OpGetValue ])
                      (BasicBlock [ compile e3, OpGetValue ]) ]


compileCall :: (Int->CompiledExpr) -> Expr -> [Expr] -> CompiledExpr
compileCall op f args =
  BasicBlock $ concatMap compileArg args
          ++ [ compile f, op (length args) ]
  where
    compileArg e = [ compile e, OpGetValue ]

compileFunCall, compileNewExpr :: Expr -> [Expr] -> CompiledExpr
compileFunCall = compileCall OpFunCall
compileNewExpr = compileCall OpNewCall

-- ref 7.8.5
compileRegExp :: String -> String -> CompiledExpr
compileRegExp r f =
  BasicBlock [ OpConst (VStr r), OpConst (VStr f),
               OpVar "RegExp", OpFunCall 2 ]