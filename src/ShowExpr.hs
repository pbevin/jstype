module ShowExpr (showProg, showExpr) where

import Data.List
import Expr

showProg :: Program -> String
showProg (Program stmts) = intercalate ";" $ map showStatement stmts

showStatement :: Statement -> String
showStatement stmt = case stmt of
  ExpressionStatement expr -> showExpr expr
  WhileStatement expr stmt -> "while" ++ parens (showExpr expr) ++ showStatement stmt
  ReturnStatement expr -> "return " ++ showExpr expr
  DebuggerStatement -> "debugger"

showExpr :: Expr -> String
showExpr expr = case expr of
  Num (JSNum n) -> show n
  Str s -> show s
  ReadVar v -> v

  MemberDot e id ->
    parens (showExpr e) ++ "." ++ id

  MemberGet a x ->
    parens (showExpr a) ++ brackets (showExpr x)

  BinOp op e1 e2 ->
    parens (showExpr e1) ++ op ++ parens (showExpr e2)

  UnOp op e ->
    op ++ parens (showExpr e)

  PostOp op e ->
    parens (showExpr e) ++ op

  Assign v op e ->
    v ++ op ++ showExpr e

  Cond test ifTrue ifFalse ->
    parens (showExpr test) ++ " ? " ++ (showExpr ifTrue) ++ " : " ++ (showExpr ifFalse)

  FunCall f x -> fun ++ params
    where fun = case f of
                  ReadVar v -> v
                  _ -> parens (showExpr f)
          params = parens (intercalate "," $ map showExpr x)

  FunDef Nothing params body ->
    "function" ++ parens (intercalate "," params) ++ braces (mapshow ";" body)

  FunDef (Just name) params body ->
    "function " ++ name ++ parens (intercalate "," params) ++ braces (mapshow ";" body)



mapshow :: String -> [Statement] -> String
mapshow sep xs = intercalate sep $ map showStatement xs

parens s = "(" ++ s ++ ")"
braces s = "{" ++ s ++ "}"
brackets s = "[" ++ s ++ "]"
