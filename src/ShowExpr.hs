module ShowExpr (showProg, showExpr) where

import Data.List
import Expr

showProg :: Program -> String
showProg (Program stmts) = intercalate ";" $ map showStatement stmts

showStatement :: Statement -> String
showStatement stmt = case stmt of
  ExprStmt expr -> showExpr expr
  WhileStatement expr stmt -> "while" ++ parens (showExpr expr) ++ showStatement stmt
  Return Nothing -> "return"
  Return (Just expr) -> "return " ++ showExpr expr
  EmptyStatement -> ";"
  DebuggerStatement -> "debugger"
  BreakStatement -> "break"
  ContinueStatement -> "continue"
  VarDecl decls -> "var " ++ showVarDecls decls
  Block statements -> braces $ intercalate "; " $ map showStatement statements

  IfStatement test ifTrue Nothing ->
    "if (" ++ showExpr test ++ ") " ++ showStatement ifTrue

  IfStatement test ifTrue (Just ifFalse) ->
    case ifTrue of
      IfStatement _ _ Nothing -> -- ambiguity case
        "if (" ++ showExpr test ++ ") { " ++ showStatement ifTrue ++ " } else " ++ showStatement ifFalse
      _ ->
        "if (" ++ showExpr test ++ ") " ++ showStatement ifTrue ++ " else " ++ showStatement ifFalse

showExpr :: Expr -> String
showExpr expr = case expr of
  Str s -> show s
  ReadVar v -> v
  This -> "this"

  Num (JSNum n)
    | isInteger n -> show (round n)
    | otherwise   -> show n

  NewExpr cls args ->
    "new " ++ maybeParens cls ++ argList args

  MemberDot e id ->
    maybeParens e ++ "." ++ id

  MemberGet a x ->
    maybeParens a ++ brackets (showExpr x)

  BinOp op e1 e2 ->
    maybeParens e1 ++ " " ++ op ++ " " ++ maybeParens e2

  UnOp op e -> op ++ maybeParens e

  PostOp op e ->
    maybeParens e ++ op

  Assign lhs op rhs ->
    showExpr lhs ++ op ++ showExpr rhs

  Cond test ifTrue ifFalse ->
    maybeParens test ++ " ? " ++ showExpr ifTrue ++ " : " ++ showExpr ifFalse

  FunCall fun args -> maybeParens fun ++ argList args

  FunDef Nothing params body ->
    "function" ++ parens (intercalate "," params) ++ braces (mapshow ";" body)

  FunDef (Just name) params body ->
    "function " ++ name ++ parens (intercalate "," params) ++ braces (mapshow ";" body)


showVarDecls :: [(String, Maybe Expr)] -> String
showVarDecls = intercalate "," . map showDecl
  where showDecl (var, expr) = var ++ maybe "" (\e -> " = " ++ showExpr e) expr

argList :: [Expr] -> String
argList args = parens (intercalate "," $ map showExpr args)


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
