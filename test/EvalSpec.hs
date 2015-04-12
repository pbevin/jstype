module EvalSpec where

import Test.Hspec

import Expr
import Eval

spec :: Spec
spec = do
  it "evaluates arithmetic" $ do
    jsEvalExpr "3+4" `shouldBe` JSNum 7
    jsEvalExpr "1+2+3" `shouldBe` JSNum 6
