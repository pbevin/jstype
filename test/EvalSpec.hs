module EvalSpec where

import Test.Hspec

import qualified Data.Map as M
import Expr
import Eval

spec :: Spec
spec = do
  it "evaluates arithmetic" $ do
    jsEvalExpr "3+4"   `shouldBe` VNum 7
    jsEvalExpr "1+2+3" `shouldBe` VNum 6
    jsEvalExpr "2*3+4" `shouldBe` VNum 10
    jsEvalExpr "2+3*4" `shouldBe` VNum 14
    jsEvalExpr "10-2"  `shouldBe` VNum 8
    jsEvalExpr "10/2"  `shouldBe` VNum 5
    jsEvalExpr "10/3"  `shouldBe` VNum 3.33333333

  it "evaluates a program" $ do
    runJS "console.log(1);" `shouldBe` Right "1\n"
    runJS "a = 3; console.log(a);" `shouldBe` Right "3\n"
    runJS "a = 3; console.log(a+4);" `shouldBe` Right "7\n"
  
  it "assigns a variable" $ do
    let (Right _, _, env) = runJS' "var a = 3"
    M.lookup "a" env `shouldBe` Just (VNum 3)

  it "does update-assignments" $ do
    runJS "a = 10; a += 1; console.log(a);" `shouldBe` Right "11\n"
    runJS "a = 10; a -= 1; console.log(a);" `shouldBe` Right "9\n"
    runJS "a = 10; a *= 3; console.log(a);" `shouldBe` Right "30\n"
    runJS "a = 10; a /= 2; console.log(a);" `shouldBe` Right "5\n"

  it "runs loops" $ do
    runJS "var t = 0, i; for (i = 0; i < 10; i++) { t += i }; console.log(t);" `shouldBe` Right "45\n"
