module Expr (Expr(..), JSNum(..), showExpr) where

import Control.Applicative
import Test.QuickCheck

newtype JSNum = JSNum Double deriving Show
instance Eq JSNum where
  JSNum a == JSNum b = abs (a-b) < 0.001

data Expr = Num JSNum
          | Str String
          | BinOp String Expr Expr
          | UnOp String Expr
  deriving (Show, Eq)


showExpr :: Expr -> String
showExpr expr = case expr of
  Num (JSNum n) -> show $ roundTo 6 n
  Str s -> show s
  BinOp op e1 e2 -> parens (showExpr e1) ++ op ++ parens (showExpr e2)
  UnOp op e -> op ++ parens (showExpr e)

parens s = "(" ++ s ++ ")"


instance Arbitrary Expr where
  arbitrary = sized arbExpr
  shrink = shrinkExpr

arbExpr 0 = oneof [ Num <$> arbNum, Str <$> arbitrary ]
arbExpr n = oneof [ BinOp <$> arbOp <*> subtree <*> subtree,
                    UnOp <$> arbUnary <*> arbExpr (n-1) ]
  where subtree = arbExpr (n `div` 2)

shrinkExpr (Num n) = [ Num (JSNum 1) ]
shrinkExpr (Str s) = [ Str "a" ]
shrinkExpr (BinOp op e1 e2) = [e1, e2]
shrinkExpr (UnOp op e) = [e]


arbOp :: Gen String
arbOp = elements [ "+", "-", "*", "/" ]
arbUnary = elements [ "+", "-", "!" ]


arbNum :: Gen JSNum
arbNum = arbitrary >>= return . JSNum . roundTo 2

roundTo :: Int -> Double -> Double
roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
