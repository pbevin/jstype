module ShowExpr (showProg, showExpr) where

import Data.List
import Expr

showProg :: Program -> String
showProg (Program stmts) = intercalate ";" $ map showStatement stmts

showStatement :: Statement -> String
showStatement stmt = case stmt of
  WhileStatement expr stmt -> "while" ++ parens (showExpr expr) ++ showStatement stmt
  Return Nothing -> "return"
  Return (Just expr) -> "return " ++ showExpr expr
  EmptyStatement -> ";"
  DebuggerStatement -> "debugger"
  BreakStatement -> "break"
  ContinueStatement -> "continue"
  VarDecl decls -> "var " ++ showVarDecls decls
  ThrowStatement expr -> "throw " ++ showExpr expr

  ExprStmt expr -> parenObjectLiterals(showExpr expr)
    where parenObjectLiterals str =
            if head str == '{'
            then parens str
            else str

  Block statements ->
    braces $ intercalate "; " $ map showStatement statements

  IfStatement test ifTrue ifFalse ->
    "if " ++ parens(showExpr test) ++ " " ++ braces (showStatement ifTrue) ++
      case ifFalse of
        Nothing -> ""
        Just stmt -> " else " ++ braces (showStatement stmt)


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

  ArrayLiteral exprs ->
    brackets (intercalate "," $ map showExpr exprs)

  ObjectLiteral assignments ->
    braces (intercalate "," $ map showAssignment assignments)

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

showAssignment :: (PropertyName, Expr) -> String
showAssignment (n, v) = showPropertyName n ++ ": " ++ showExpr v

showPropertyName (IdentProp p) = p
showPropertyName (StringProp p) = show p
showPropertyName (NumProp (JSNum n)) = show n

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
