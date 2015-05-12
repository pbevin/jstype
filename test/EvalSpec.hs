module EvalSpec where

import Test.Hspec

import qualified Data.Map as M
import Expr
import Eval

spec :: Spec
spec = do
  it "evaluates arithmetic" $ do
    jsEvalExpr "3+4"   `shouldReturn` VNum 7
    jsEvalExpr "1+2+3" `shouldReturn` VNum 6
    jsEvalExpr "2*3+4" `shouldReturn` VNum 10
    jsEvalExpr "2+3*4" `shouldReturn` VNum 14
    jsEvalExpr "10-2"  `shouldReturn` VNum 8
    jsEvalExpr "10/2"  `shouldReturn` VNum 5
    jsEvalExpr "10/3"  `shouldReturn` VNum 3.33333333

  it "adds two strings" $ do
    jsEvalExpr "\"a\" + \"b\"" `shouldReturn` VStr "ab"

  it "evaluates a program" $ do
    runJS "console.log(1);" `shouldReturn` Right "1\n"
    runJS "a = 3; console.log(a);" `shouldReturn` Right "3\n"
    runJS "a = 3; console.log(a+4);" `shouldReturn` Right "7\n"
  
  it "does update-assignments" $ do
    runJS "a = 10; a += 1; console.log(a);" `shouldReturn` Right "11\n"
    runJS "a = 10; a -= 1; console.log(a);" `shouldReturn` Right "9\n"
    runJS "a = 10; a *= 3; console.log(a);" `shouldReturn` Right "30\n"
    runJS "a = 10; a /= 2; console.log(a);" `shouldReturn` Right "5\n"

  it "runs loops" $ do
    runJS "var t = 0, i; for (i = 0; i < 10; i++) { t += i }; console.log(t);" `shouldReturn` Right "45\n"

  it "can call a function" $ do
    runJS "function print(msg) { console.log(msg); }; print(\"hi\")" `shouldReturn` Right "hi\n"

  it "can define a simple object" $ do
    runJS "function A() { this.a = 0; }; A.prototype.inc = function() { this.a++ }; var a = new A(); a.inc(); console.log(a.a);" `shouldReturn` Right "1\n"
