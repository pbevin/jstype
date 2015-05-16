module EvalSpec where

import Test.Hspec

import qualified Data.Map as M
import Expr
import Eval
import Runtime.Types

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
    runJStr "console.log(1);" `shouldReturn` Right "1\n"
    runJStr "a = 3; console.log(a);" `shouldReturn` Right "3\n"
    runJStr "a = 3; console.log(a+4);" `shouldReturn` Right "7\n"
  
  it "does update-assignments" $ do
    runJStr "a = 10; a += 1; console.log(a);" `shouldReturn` Right "11\n"
    runJStr "a = 10; a -= 1; console.log(a);" `shouldReturn` Right "9\n"
    runJStr "a = 10; a *= 3; console.log(a);" `shouldReturn` Right "30\n"
    runJStr "a = 10; a /= 2; console.log(a);" `shouldReturn` Right "5\n"

  it "runs loops" $ do
    runJStr "var t = 0, i; for (i = 0; i < 10; i++) { t += i }; console.log(t);" `shouldReturn` Right "45\n"

  it "runs do-while loops" $ do
    runJStr "var t = 3; do { console.log(t--) } while (t > 0);"
      `shouldReturn` Right "3\n2\n1\n"

  it "can call a function" $ do
    runJStr "function print(msg) { console.log(msg); }; print(\"hi\")" `shouldReturn` Right "hi\n"

  it "can set an object property" $ do
    runJStr "a = function() { }; a.prop = 2; console.log(a.prop);" `shouldReturn` Right "2\n"

  it "can do if-then-else" $ do
    runJStr "if (1) { console.log(\"hi\") }" `shouldReturn` Right "hi\n"
    runJStr "if (0) { console.log(\"wrong\") } else { console.log(\"yes\") } " `shouldReturn` Right "yes\n"

  it "can do typeof" $ do
    runJStr "console.log(typeof console)" `shouldReturn` Right "object\n"
    runJStr "console.log(typeof this)" `shouldReturn` Right "object\n"
    runJStr "console.log(typeof 5)" `shouldReturn` Right "number\n"
    runJStr "console.log(typeof 'aa')" `shouldReturn` Right "string\n"


  it "can define a function" $ do
    runJStr "f = function() { return 2; }; console.log(f())" `shouldReturn` Right "2\n"

  it "can define a function via Function(...)" $ do
    runJStr "f = Function('return 3'); console.log(f());" `shouldReturn` Right "3\n"

  it "can immediately invoke a constructed function" $ do
    runJStr "console.log(Function('return 42')())" `shouldReturn` Right "42\n"

  it "can do ===" $ do
    runJStr "if (1 === 1) console.log(\"OK\")" `shouldReturn` Right "OK\n"
    runJStr "if (1 === 0) console.log(\"wrong\")" `shouldReturn` Right ""
    runJStr "if ('a' === 'b') console.log(\"wrong\")" `shouldReturn` Right ""
    runJStr "if ('a' === 'a') console.log(\"OK\")" `shouldReturn` Right "OK\n"
    runJStr "if (console === console) console.log(\"OK\")" `shouldReturn` Right "OK\n"
    runJStr "if (console === this) console.log(\"wrong\")" `shouldReturn` Right ""

  it "can define a simple object" $ do
    runJStr "function Counter() { this.val = 0; }; Counter.prototype.inc = function() { this.val++ }; var counter = new Counter(); counter.inc(); counter.inc(); console.log(counter.val);" `shouldReturn` Right "2\n"
