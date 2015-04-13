module ShowExprSpec where

import Test.Hspec
import Test.QuickCheck

import Eval
import Expr
import ShowExpr

spec :: Spec
spec = do
  it "shows a number" $ do
    showExpr (Num 1.5) `shouldBe` "1.5"
    showExpr (Num 1) `shouldBe` "1"

  it "shows a unary op" $ do
    showExpr (UnOp "-" (Num 1)) `shouldBe` "-1"
    showExpr (UnOp "-" (ReadVar "a")) `shouldBe` "-a"
    showExpr (UnOp "-" (BinOp "+" (ReadVar "a") (ReadVar "b"))) `shouldBe` "-(a + b)"

  it "shows a function call" $ do
    showExpr (FunCall (ReadVar "f") [ReadVar "x"]) `shouldBe` "f(x)"
    showExpr (FunCall (BinOp "||" (ReadVar "f") (ReadVar "g")) [ReadVar "x"]) `shouldBe` "(f || g)(x)"
