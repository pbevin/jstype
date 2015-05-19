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

  it "evaluates a program" $ do
    runJStr "console.log(1);" `shouldReturn` Right "1\n"
    runJStr "a = 3; console.log(a);" `shouldReturn` Right "3\n"
    runJStr "a = 3; console.log(a+4);" `shouldReturn` Right "7\n"
  
  it "does update-assignments" $ do
    runJStr "a = 10; a += 1; console.log(a);" `shouldReturn` Right "11\n"
    runJStr "a = 10; a -= 1; console.log(a);" `shouldReturn` Right "9\n"
    runJStr "a = 10; a *= 3; console.log(a);" `shouldReturn` Right "30\n"
    runJStr "a = 10; a /= 2; console.log(a);" `shouldReturn` Right "5\n"

  it "does +, - and void prefixes" $ do
    runJStr "var a = '5'; console.log(+a); console.log(a)"
      `shouldReturn` Right "5\n5\n"
    runJStr "var a = '5'; console.log(-a); console.log(a)"
      `shouldReturn` Right "-5\n5\n"
    runJStr "var a = '5'; console.log(void a); console.log(a)"
      `shouldReturn` Right "(undefined)\n5\n"

  it "does ! prefix" $ do
    runJStr "console.log(!true); console.log(!false)"
      `shouldReturn` Right "false\ntrue\n"

  it "does pre-increment and pre-decrement" $ do
    runJStr "var a = 5; console.log(--a); console.log(a)"
      `shouldReturn` Right "4\n4\n"
    runJStr "var a = 5; console.log(++a); console.log(a)"
      `shouldReturn` Right "6\n6\n"

  it "does post-increment and post-decrement" $ do
    runJStr "var a = 5; console.log(a--); console.log(a)"
      `shouldReturn` Right "5\n4\n"
    runJStr "var a = 5; console.log(a++); console.log(a)"
      `shouldReturn` Right "5\n6\n"

  describe "an empty array" $ do
    it "has length 0" $ do
      jsEvalExpr "[].length" `shouldReturn` VNum 0

    it "has type object" $ do
      jsEvalExpr "typeof []" `shouldReturn` VStr "object"

  describe "an array with 5 elements" $ do
    it "has length 5" $ do
      jsEvalExpr "[1,2,3,4,5].length" `shouldReturn` VNum 5

  describe "an array with 3 elisions" $ do
    it "has length 3" $ do
      jsEvalExpr "[,,].length" `shouldReturn` VNum 3

  describe "comparison" $ do
    it "understands <" $ do
      jsEvalExpr "1 < 2" `shouldReturn` VBool True
      jsEvalExpr "2 < 1" `shouldReturn` VBool False
      jsEvalExpr "1 < 1" `shouldReturn` VBool False

    it "understands <=" $ do
      jsEvalExpr "1 <= 2" `shouldReturn` VBool True
      jsEvalExpr "2 <= 1" `shouldReturn` VBool False
      jsEvalExpr "1 <= 1" `shouldReturn` VBool True

    it "understands >" $ do
      jsEvalExpr "1 > 2" `shouldReturn` VBool False
      jsEvalExpr "2 > 1" `shouldReturn` VBool True
      jsEvalExpr "1 > 1" `shouldReturn` VBool False

    it "understands >=" $ do
      jsEvalExpr "1 >= 2" `shouldReturn` VBool False
      jsEvalExpr "2 >= 1" `shouldReturn` VBool True
      jsEvalExpr "1 >= 1" `shouldReturn` VBool True

    it "understands abstract equality with numbers" $ do
      jsEvalExpr "1 == 1" `shouldReturn` VBool True
      jsEvalExpr "1 == 2" `shouldReturn` VBool False

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

  it "can create a new object" $ do
    runJStr "var x = new Object(); console.log(x)" `shouldReturn` Right "[Object object]\n"

  it "runs a try..catch block" $ do
    runJStr "try { throw new Error('hi') } catch (e) { console.log(e.message); }"
    `shouldReturn` Right "hi\n";

  it "raises runtime exceptions" $ do
    Left (message, _stack) <- runJStr "var a; a();"
    message `shouldBe` "Can't call undefined"

  describe "The eval function" $ do
    it "can eval code" $ do
      runJStr "eval(\"console.log('hi')\")" `shouldReturn` Right "hi\n"

    it "treats white space with respect" $ do
      -- test262: 11.6.1_A1
      jsEvalExpr "eval(\"1\\u0009\\u000B\\u000C\\u0020\\u00A0\\u000A\\u000D\\u2028\\u2029+\\u0009\\u000B\\u000C\\u0020\\u00A0\\u000A\\u000D\\u2028\\u20291\")" `shouldReturn` VNum 2

  describe "Number" $ do
    it "has a NaN property" $ do
      runJStr "console.log(Number.NaN)" `shouldReturn` Right "NaN\n"

    it "can construct a wrapped number" $ do
      runJStr "console.log(+(new Number(1.4)))" `shouldReturn` Right "1.4\n"
