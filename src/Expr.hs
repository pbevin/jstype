module Expr (Expr(..), JSNum(..), Lang(..), jsLang, showExpr) where

import Control.Applicative
import Test.QuickCheck

newtype JSNum = JSNum Double deriving Show
instance Eq JSNum where
  JSNum a == JSNum b = abs (a-b) < 0.001

data Expr = Num JSNum
          | Str String
          | BinOp String Expr Expr
          | UnOp String Expr
          | PostOp String Expr
          | ReadVar String
          | Assign String String Expr
          | Cond Expr Expr Expr
  deriving (Show, Eq)

data Lang = Lang {
  assignOps :: [String],
  unaryOps :: [String],
  binaryOps :: [String],
  postfixOps :: [String]
}

jsLang :: Lang
jsLang = Lang {
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


showExpr :: Expr -> String
showExpr expr = case expr of
  Num (JSNum n) -> show n
  Str s -> show s
  BinOp op e1 e2 ->
    parens (showExpr e1) ++ op ++ parens (showExpr e2)
  UnOp op e -> op ++ parens (showExpr e)
  PostOp op e -> parens (showExpr e) ++ op
  ReadVar v -> v
  Assign v op e -> v ++ op ++ showExpr e
  Cond test ifTrue ifFalse ->
    parens (showExpr test) ++ " ? " ++ (showExpr ifTrue) ++ " : " ++ (showExpr ifFalse)

parens s = "(" ++ s ++ ")"


instance Arbitrary Expr where
  arbitrary = sized arbExpr
  shrink = shrinkExpr

arbExpr 0 = oneof [ Num <$> arbNum,
                    Str <$> arbitrary,
                    ReadVar <$> arbVar ]
arbExpr n = oneof [ BinOp <$> arbOp <*> subtree <*> subtree,
                    UnOp <$> arbUnary <*> subtree,
                    PostOp <$> arbPostfix <*> subtree,
                    Assign <$> arbVar <*> arbAssignOp <*> arbExpr (n-1),
                    Cond <$> subtree <*> subtree <*> subtree ]
  where subtree = arbExpr (n `div` 2)

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

arbVar :: Gen String
arbVar = do
  x <- elements ['a'..'z']
  xs <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['_']
  return (x:xs)



shrinkExpr (BinOp op e1 e2) = [e1, e2] ++ [BinOp op e1' e2' | e1' <- shrinkExpr e1, e2' <- shrinkExpr e2]
shrinkExpr (UnOp op e) = [e] ++ [UnOp op e' | e' <- shrinkExpr e]
shrinkExpr (Assign v@[ch] op e) = [Assign v op e' | e' <- shrinkExpr e] ++ [e]
shrinkExpr (Assign v@(ch:_) op e) = [Assign [ch] op e]
shrinkExpr (Num (JSNum n))
  | n == 0 = []
  | otherwise = [Num (JSNum 0)]
shrinkExpr (Cond a b c) = [a, b, c] ++
  [Cond a' b' c' | a' <- shrinkExpr a, b' <- shrinkExpr b, c' <- shrinkExpr c]
shrinkExpr (ReadVar [ch]) = []
shrinkExpr (ReadVar (ch:_)) = [ReadVar [ch]]
shrinkExpr (Str "") = []
shrinkExpr (Str _) = [Str ""]
shrinkExpr _ = []
