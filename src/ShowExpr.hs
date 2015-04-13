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
  VarDecl decls -> "var " ++ showVarDecls decls

showExpr :: Expr -> String
showExpr expr = case expr of
  Str s -> show s
  ReadVar v -> v

  Num (JSNum n)
    | isInteger n -> show (round n)
    | otherwise   -> show n

  MemberDot e id ->
    maybeParens e ++ "." ++ id

  MemberGet a x ->
    maybeParens a ++ brackets (showExpr x)

  BinOp op e1 e2 ->
    maybeParens e1 ++ " " ++ op ++ " " ++ maybeParens e2

  UnOp op e -> op ++ maybeParens e

  PostOp op e ->
    maybeParens e ++ op

  Assign v op e ->
    v ++ op ++ showExpr e

  Cond test ifTrue ifFalse ->
    maybeParens test ++ " ? " ++ showExpr ifTrue ++ " : " ++ showExpr ifFalse

  FunCall f x -> fun ++ params
    where fun = maybeParens f
          params = parens (intercalate "," $ map showExpr x)

  FunDef Nothing params body ->
    "function" ++ parens (intercalate "," params) ++ braces (mapshow ";" body)

  FunDef (Just name) params body ->
    "function " ++ name ++ parens (intercalate "," params) ++ braces (mapshow ";" body)


showVarDecls :: [(String, Expr)] -> String
showVarDecls = intercalate "," . map showDecl
  where showDecl (var, expr) = var ++ " = " ++ showExpr expr



mapshow :: String -> [Statement] -> String
mapshow sep xs = intercalate sep $ map showStatement xs

parens s = "(" ++ s ++ ")"
braces s = "{" ++ s ++ "}"
brackets s = "[" ++ s ++ "]"

isInteger :: RealFrac a => a -> Bool
isInteger x = x == fromIntegral (round x :: Integer)

maybeParens :: Expr -> String
maybeParens e = case e of
  Num _ -> showExpr e
  ReadVar _ -> showExpr e
  _ -> parens (showExpr e)
