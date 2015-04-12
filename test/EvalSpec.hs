module EvalSpec where

import Test.Hspec

import Expr
import Eval

spec :: Spec
spec = do
  it "evaluates arithmetic" $ do
    jsEvalExpr "3+4" `shouldBe` 7
    jsEvalExpr "1+2+3" `shouldBe` 6
    jsEvalExpr "2*3+4" `shouldBe` 10
    jsEvalExpr "2+3*4" `shouldBe` 14
    jsEvalExpr "10-2" `shouldBe` 8
    jsEvalExpr "10/2" `shouldBe` 5
    jsEvalExpr "10/3" `shouldBe` 3.33333333

  -- it "evaluates a program" $ do
  --   jsEval "var a = 1; console.log(a);" `shouldBe` Just "1\n"
