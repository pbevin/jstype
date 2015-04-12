module Expr (Program (..),
             Statement (..),
             Expr(..),
             JSNum(..),
             Lang(..),
             jsLang,
             showProg,
             showExpr) where

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Data.List

newtype JSNum = JSNum Double deriving Show
instance Eq JSNum where
  JSNum a == JSNum b = abs (a-b) < 0.001

newtype Program = Program [Statement] deriving (Show, Eq)

type Ident = String
type Operator = String
type ParameterList = [Ident]
type VarDeclaration = [(Ident, Expr)]

data ForHeader = For3 Expr Expr Expr
               | For3Var Ident Expr Expr Expr
               | ForIn LHS Expr
               | ForInVar Ident LHS Expr
  deriving (Show, Eq)

data Statement = Block [Statement]
               | Var [VarDeclaration]
               | EmptyStatement
               | ExpressionStatement Expr
               | IfStatement
               | WhileStatement Expr Statement
               | DoWhileStatement Expr Statement
               | For ForHeader Statement
               -- | ForStatement ... (4 cases)
               | ContinueStatement
               | BreakStatement
               | ReturnStatement
               -- | WithStatement
               | IdentifierStatement Ident Statement
               | SwitchStatement
               | ThrowStatement
               | TryStatement
               | DebuggerStatement
  deriving (Show, Eq)

type FunBody = [Statement]

type LHS = Expr -- XXX

data Expr = Num JSNum
          | Str String
          | BinOp Operator Expr Expr
          | UnOp Operator Expr
          | PostOp Operator Expr
          | ReadVar Ident
          | Assign Ident String Expr
          | Cond Expr Expr Expr
          | FunCall Expr [Expr]
          | FunDef (Maybe Ident) ParameterList FunBody
  deriving (Show, Eq)

data Lang = Lang {
  reservedWords :: [String],
  assignOps :: [String],
  unaryOps :: [String],
  binaryOps :: [String],
  postfixOps :: [String]
}

jsLang :: Lang
jsLang = Lang {
  reservedWords  = [ "var", "let", "while", "in", "if", "debugger" ],
  assignOps = [ "=", "+=", "-=", "*=", "/=", "%=",
                "<<=", ">>=", ">>>=", "&=", "^=", "|="],
  unaryOps  = [ "delete", "void", "typeof",
               "+", "-", "~", "!", "++", "--" ],
  binaryOps = [ "+", "-", "*", "/", "%", "==", "!=", "===", "!==",
                "&", "^", "|", "&&", "||",
                "in", "instanceof", ">>", "<<", ">>>",
                ">=", ">", "<=", "<" ],
  postfixOps = [ "++", "--" ]
}

showProg :: Program -> String
showProg (Program stmts) = intercalate ";" $ map showStatement stmts

showStatement :: Statement -> String
showStatement stmt = case stmt of
  ExpressionStatement expr -> showExpr expr
  WhileStatement expr stmt -> "while" ++ parens (showExpr expr) ++ showStatement stmt
  EmptyStatement -> ";"
  DebuggerStatement -> "debugger"

showExpr :: Expr -> String
showExpr expr = case expr of
  Num (JSNum n) -> show n
  Str s -> show s
  ReadVar v -> v
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




instance Arbitrary Program where
  arbitrary = sized arbProg
  shrink = shrinkProg

instance Arbitrary Statement where
  arbitrary = sized arbStmt
  shrink = shrinkStmt

instance Arbitrary Expr where
  arbitrary = sized arbExpr
  shrink = shrinkExpr

arbProg :: Int -> Gen Program
arbProg n = Program <$> shortListOf n arbitrary

arbStmt :: Int -> Gen Statement
arbStmt n = oneof [ ExpressionStatement <$> arbExpr n,
                    WhileStatement <$> arbExpr half <*> arbStmt half,
                    pure EmptyStatement,
                    pure DebuggerStatement ]
  where half = n `div` 2




arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [ Num <$> arbNum,
                    Str <$> arbitrary,
                    ReadVar <$> arbIdent ]
arbExpr n = oneof [ BinOp <$> arbOp <*> subexpr <*> subexpr,
                    UnOp <$> arbUnary <*> subexpr,
                    PostOp <$> arbPostfix <*> subexpr,
                    Assign <$> arbIdent <*> arbAssignOp <*> resize (n-1) arbitrary,
                    FunCall <$> subexpr <*> shortListOf half arbitrary,
                    FunDef Nothing <$> listOf arbIdent <*> pure [],
                    FunDef <$>
                      (Just <$> arbIdent) <*>
                      listOf arbIdent <*>
                      shortListOf n arbitrary,
                    Cond <$> subexpr <*> subexpr <*> subexpr ]
  where subexpr = arbExpr half
        half = n `div` 2

arbOp :: Gen String
arbOp = elements (binaryOps jsLang)

arbUnary :: Gen String
arbUnary = elements (unaryOps jsLang)

arbPostfix :: Gen String
arbPostfix = elements (postfixOps jsLang)

arbNum :: Gen JSNum
arbNum = arbitrary >>= return . JSNum . getNonNegative

arbAssignOp :: Gen String
arbAssignOp = elements $ assignOps jsLang

arbIdent :: Gen Ident
arbIdent = arbVarName `suchThat` notReserved
  where notReserved name = not $ name `elem` reservedWords jsLang

arbVarName :: Gen Ident
arbVarName = do
  x <- elements ['a'..'z']
  xs <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['_']
  return (x:xs)

shortListOf :: Int -> Gen a -> Gen [a]
shortListOf n a = do
  let max = floor (sqrt $ fromIntegral n)
  len <- choose(0, max)
  vectorOf len $ resize max a


shrinkProg :: Program -> [Program]
shrinkProg (Program stmts) = [Program p | p <- recursivelyShrink stmts]

shrinkStmt :: Statement -> [Statement]
shrinkStmt expr = case expr of
  ExpressionStatement e -> [ExpressionStatement e | e <- shrink e]
  WhileStatement e s ->
    [ExpressionStatement e, s] ++ [WhileStatement e' s' | e' <- shrink e, s' <- shrink s]

  _ -> []


shrinkExpr :: Expr -> [Expr]
shrinkExpr expr = case expr of
  Str "" -> []
  Str _ -> [Str ""]
  ReadVar [ch] -> []
  ReadVar (ch:_) -> [ReadVar [ch]]
  Num (JSNum n)
    | n == 0 -> []
    | otherwise -> [Num (JSNum 0)]

  BinOp op e1 e2 ->
    [e1, e2] ++ [BinOp op e1' e2' | e1' <- shrinkExpr e1, e2' <- shrinkExpr e2]

  UnOp op e ->
    [e] ++ [UnOp op e' | e' <- shrinkExpr e]

  PostOp op e ->
    [e] ++ [PostOp op e' | e' <- shrinkExpr e]

  Assign v@[ch] op e ->
    [Assign v op e' | e' <- shrinkExpr e] ++ [e]

  Assign v@(ch:_) op e ->
    [Assign [ch] op e]

  Cond a b c ->
    [a, b, c] ++ [Cond a' b' c' | a' <- shrinkExpr a, b' <- shrinkExpr b, c' <- shrinkExpr c]

  FunCall f xs ->
    [f] ++ xs ++
      [FunCall f' xs | f' <- shrinkExpr f] ++
      [FunCall f xs' | xs' <- shrinkList shrinkExpr xs]

  FunDef name params body ->
    case params of
      []          -> []
      (_:params') -> [ FunDef name params' body ]
