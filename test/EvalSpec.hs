{-# LANGUAGE LambdaCase #-}

module EvalSpec where

import Test.Hspec

import Expr
import Eval
import Runtime
import Expectations


runJStr :: String -> IO (Either RuntimeError String)
runJStr = runJS ""


spec :: Spec
spec = do
  it "evaluates arithmetic" $ do
    jsEvalExpr "3+4"   `shouldReturn` VNum 7
    jsEvalExpr "1+2+3" `shouldReturn` VNum 6
    jsEvalExpr "2*3+4" `shouldReturn` VNum 10
    jsEvalExpr "2+3*4" `shouldReturn` VNum 14
    jsEvalExpr "10-2"  `shouldReturn` VNum 8
    jsEvalExpr "10/2"  `shouldReturn` VNum 5
    jsEvalExpr "10/3"  `shouldReturn` VNum (10/3)

  it "does Javascript-style mod for negative numbers" $ do
    jsEvalExpr "-1 % 2" `shouldReturn` VNum (-1)

  it "evaluates a program" $ do
    runJStr "console.log(1);" `shouldReturn` Right "1\n"
    runJStr "var a = 3; console.log(a);" `shouldReturn` Right "3\n"
    runJStr "var a = 3; console.log(a+4);" `shouldReturn` Right "7\n"

  it "does update-assignments" $ do
    runJStr "var a = 10; a += 1; console.log(a);" `shouldReturn` Right "11\n"
    runJStr "var a = 10; a -= 1; console.log(a);" `shouldReturn` Right "9\n"
    runJStr "var a = 10; a *= 3; console.log(a);" `shouldReturn` Right "30\n"
    runJStr "var a = 10; a /= 2; console.log(a);" `shouldReturn` Right "5\n"

  describe "Strict mode" $ do
    it "can assign an undeclared var in non-strict mode" $ do
      runJStr "a = 10; console.log(a)" `shouldReturn` Right "10\n"

    it "will not assign an undeclared var in strict mode" $ do
      runJStr "'use strict'; a = 10; console.log(a)" `shouldError` "ReferenceError: a is not defined"

    it "will not assign to a var called 'eval'" $ do
      runJStr "'use strict'; var eval = 42;" `shouldError` "SyntaxError: Assignment of eval in strict mode"

    it "will not assign to a var called 'arguments'" $ do
      runJStr "'use strict'; var arguments = 42;" `shouldError` "SyntaxError: Assignment of arguments in strict mode"

    it "applies to an eval in a strict function" $ do
      let prog = unlines [ "function f() {",
                           "  'use strict';",
                           "  eval('var public = 1; console.log(\"no\");');",
                           "}",
                           "f();" ]
      runJStr prog `shouldError` "SyntaxError: \"(eval)\" (line 1, column 11):\nunexpected reserved word public\nexpecting var declaration"

    it "applies to an eval in a strict function (case 2)" $ do
      let prog = unlines [ "function testcase() {",
                           "    \"use strict\";",
                           "    try {",
                           "        eval(\"_11_13_2_1 *= 1;\");",
                           "        console.log('should not be here');",
                           "        return false;",
                           "    } catch (e) {",
                           "        return e instanceof ReferenceError;",
                           "    }",
                           "}",
                           "if (testcase()) { console.log('ok') }" ]
      runJStr prog `shouldReturn` Right "ok\n"


  it "does +, - and void prefixes" $ do
    runJStr "var a = '5'; console.log(+a); console.log(a)"
      `shouldReturn` Right "5\n5\n"
    runJStr "var a = '5'; console.log(-a); console.log(a)"
      `shouldReturn` Right "-5\n5\n"
    runJStr "var a = '5'; console.log(void a); console.log(a)"
      `shouldReturn` Right "undefined\n5\n"

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

  it "evaluates a primitive number wrapped in an object" $ do
    jsEvalExpr "{valueOf: function() {return 1}} + 1" `shouldReturn` VNum 2

  it "evaluates date objects specially" $ do
    runJStr "var date = new Date(); console.log(date + date == date.toString() + date.toString())"
      `shouldReturn` Right "true\n"

  it "can add a number and an object" $ do
    runJStr "console.log(1 + new Object())" `shouldReturn` Right "1[object Object]\n"

  describe "Object literal" $ do
    it "can have a getter" $ do
      let prog = unlines [
                  "  var obj = { ",
                  "    get x() { return 1; } ",
                  "  }; ",
                  "  console.log(obj.x); " ]
      runJStr prog `shouldReturn` Right "1\n"


  describe "an empty array" $ do
    it "has length 0" $ do
      jsEvalExpr "[].length" `shouldReturn` VNum 0

    it "has type object" $ do
      jsEvalExpr "typeof []" `shouldReturn` VStr "object"

  describe "an array with 5 elements" $ do
    it "has length 5" $ do
      jsEvalExpr "[1,2,3,4,5].length" `shouldReturn` VNum 5
    it "has properties for each of its indices" $ do
      jsEvalExpr "[1,2,3,4,5][0]" `shouldReturn` VNum 1
      jsEvalExpr "[1,2,3,4,5][1]" `shouldReturn` VNum 2
      jsEvalExpr "[1,2,3,4,5][2]" `shouldReturn` VNum 3
      jsEvalExpr "[1,2,3,4,5][3]" `shouldReturn` VNum 4
      jsEvalExpr "[1,2,3,4,5][4]" `shouldReturn` VNum 5

  describe "an array with 1 elision" $ do
    it "has length 1" $ do
      jsEvalExpr "[,].length" `shouldReturn` VNum 1

  describe "an array with 2 elisions" $ do
    it "has length 2" $ do
      jsEvalExpr "[,,].length" `shouldReturn` VNum 2

  describe "an array with many elisions at the end" $ do
    it "has the right length" $ do
      jsEvalExpr "[4,5].length" `shouldReturn` VNum 2
      jsEvalExpr "[4,5,].length" `shouldReturn` VNum 2
      jsEvalExpr "[4,5,,].length" `shouldReturn` VNum 3
      jsEvalExpr "[4,5,,,].length" `shouldReturn` VNum 4
      jsEvalExpr "[4,5,,,,].length" `shouldReturn` VNum 5

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

  it "does bitshift operations" $ do
    jsEvalExpr "3 << 2" `shouldReturn` VNum 12
    jsEvalExpr "12 >> 2" `shouldReturn` VNum 3

  describe "left shift" $ do
    it "does hard case 5" $ do
      jsEvalExpr "6442450943.1 << 0" `shouldReturn` VNum 2147483647
    it "does hard case 6" $ do
      jsEvalExpr "-2147483649.1 << 0" `shouldReturn` VNum 2147483647

  it "runs loops" $ do
    runJStr "var t = 0, i; for (i = 0; i < 10; i++) { t += i }; console.log(t);" `shouldReturn` Right "45\n"

  it "runs do-while loops" $ do
    runJStr "var t = 3; do { console.log(t--) } while (t > 0);"
      `shouldReturn` Right "3\n2\n1\n"

  it "can call a function" $ do
    runJStr "function print(msg) { console.log(msg); }; print(\"hi\")" `shouldReturn` Right "hi\n"

  it "can set an object property" $ do
    runJStr "var a = function() { }; a.prop = 2; console.log(a.prop);" `shouldReturn` Right "2\n"

  it "can do if-then-else" $ do
    runJStr "if (1) { console.log(\"hi\") }" `shouldReturn` Right "hi\n"
    runJStr "if (0) { console.log(\"wrong\") } else { console.log(\"yes\") } " `shouldReturn` Right "yes\n"

  it "can do typeof" $ do
    runJStr "console.log(typeof console)" `shouldReturn` Right "object\n"
    runJStr "console.log(typeof this)" `shouldReturn` Right "object\n"
    runJStr "console.log(typeof 5)" `shouldReturn` Right "number\n"
    runJStr "console.log(typeof 'aa')" `shouldReturn` Right "string\n"

  it "can still typeof a variable that doesn't exist" $ do
    jsEvalExpr "typeof x" `shouldReturn` VStr "undefined"

  it "can define a function" $ do
    runJStr "var f = function() { return 2; }; console.log(f())" `shouldReturn` Right "2\n"

  it "can define a function via Function(...)" $ do
    runJStr "var f = Function('return 3'); console.log(f());" `shouldReturn` Right "3\n"

  it "can immediately invoke a constructed function" $ do
    runJStr "console.log(Function('return 42')())" `shouldReturn` Right "42\n"

  it "can do ===" $ do
    runJStr "if (1 === 1) console.log(\"OK\")" `shouldReturn` Right "OK\n"
    runJStr "if (1 === 0) console.log(\"wrong\")" `shouldReturn` Right ""
    runJStr "if ('a' === 'b') console.log(\"wrong\")" `shouldReturn` Right ""
    runJStr "if ('a' === 'a') console.log(\"OK\")" `shouldReturn` Right "OK\n"
    runJStr "if (console === console) console.log(\"OK\")" `shouldReturn` Right "OK\n"
    runJStr "if (console === this) console.log(\"wrong\")" `shouldReturn` Right ""

  it "can call a method on true" $ do
    jsEvalExpr "true.toString()" `shouldReturn` VStr "true"

  it "can define a simple object" $ do
    let prog = unlines [
          "function Counter() { this.val = 0; };",
          "Counter.prototype.inc = function() { this.val++ };",
          "var counter = new Counter();",
          "counter.inc();",
          "counter.inc();",
          "console.log(counter.val);" ]

    runJStr prog `shouldReturn` Right "2\n"

  it "can create a new object" $ do
    runJStr "var x = new Object(); console.log(x)" `shouldReturn` Right "[Object object]\n"

  it "runs a try..catch block" $ do
    runJStr "try { throw new Error('hi') } catch (e) { console.log(e.message); }"
    `shouldReturn` Right "hi\n";

  it "raises an error to the top level" $ do
    runJStr "throw 'hi'" `shouldError` "hi"

  it "evaluates || properly" $ do
    runJStr "console.log(1 || 2)" `shouldReturn` Right "1\n"
    runJStr "console.log(false || 2)" `shouldReturn` Right "2\n"

  it "raises runtime exceptions" $ do
    runJStr "var a; a();" `shouldError` "ReferenceError: Function a is undefined"

  it "can throw an exception from a function" $ do
    let prog = unlines [
                 " function f(m) { throw new Error(m); } " ,
                 " f('abc'); " ]
    runJStr prog `shouldError` "Error: abc"


  describe "The eval function" $ do
    it "can eval code" $ do
      runJStr "eval(\"console.log('hi')\")" `shouldReturn` Right "hi\n"

    it "treats white space with respect" $ do
      -- test262: 11.6.1_A1
      jsEvalExpr "eval(\"1\\u0009\\u000B\\u000C\\u0020\\u00A0\\u000A\\u000D\\u2028\\u2029+\\u0009\\u000B\\u000C\\u0020\\u00A0\\u000A\\u000D\\u2028\\u20291\")" `shouldReturn` VNum 2

    it "throws a SyntaxError if parsing fails" $ do
      runJStr "try { eval('var'); } catch (e) { if (e instanceof SyntaxError) { console.log('OK 1') } }" `shouldReturn` Right "OK 1\n"

    it "does not throw SyntaxError if the code fails to run" $ do
      runJStr "try { eval('x.a.b'); } catch (e) { if (e instanceof SyntaxError) {} else { console.log('OK 2') } }" `shouldReturn` Right "OK 2\n"

    it "throws an error to its caller" $ do
      runJStr "try { eval(\"'use strict'; a = 1;\") } catch (e) { console.log('ok'); if (e instanceof ReferenceError) { console.log(e.message); } }"
        `shouldReturn` Right "ok\na is not defined\n"

  describe "Boolean" $ do
    it "can be called as a function" $ do
      jsEvalExpr "Boolean(true)" `shouldReturn` VBool True
    it "can be called as a constructor" $ do
      v <- jsEvalExpr "new Boolean(true)"
      v `shouldSatisfy` isObj

  describe "Number" $ do
    it "has a NaN property" $ do
      runJStr "console.log(Number.NaN)" `shouldReturn` Right "NaN\n"

    it "can construct a wrapped number" $ do
      runJStr "console.log(+(new Number(1.4)))" `shouldReturn` Right "1.4\n"

    it "has toFixed()" $ do
      runJStr "var a = 2.3; console.log(a.toFixed(2));" `shouldReturn` Right "2.30\n"
      runJStr "var a = 2.3; console.log(a.toFixed(5));" `shouldReturn` Right "2.30000\n"
      runJStr "var a = 2.335; console.log(a.toFixed(2));" `shouldReturn` Right "2.34\n"

  describe "Math" $ do
    it "understands infinity and NaN" $ do
      runJStr "console.log(Infinity)" `shouldReturn` Right "Infinity\n"
      runJStr "console.log(-Infinity)" `shouldReturn` Right "-Infinity\n"
      runJStr "console.log(Math.abs(-Infinity))" `shouldReturn` Right "Infinity\n"
      runJStr "console.log(NaN)" `shouldReturn` Right "NaN\n"

    it "understands negative zero" $ do
      runJStr "console.log(1 / -0)" `shouldReturn` Right "-Infinity\n"

  describe "Object" $ do
    it "can define a property with a constant value" $ do
      let prog = unlines [
                  "  var obj = {}; ",
                  "  Object.defineProperty(obj, 'prop', { ",
                  "    value: 'cheese' ",
                  "  }); ",
                  "  console.log(obj.prop);" ]
      runJStr prog `shouldReturn` Right "cheese\n"


    it "can define a property with a getter" $ do
      let prog = unlines [
                  "  var obj = {}; ",
                  "  Object.defineProperty(obj, 'prop', { ",
                  "    get: function () { ",
                  "      return 'abc';  ",
                  "    } ",
                  "  }); ",
                  "  console.log(obj.prop);" ]
      runJStr prog `shouldReturn` Right "abc\n"

  describe "Array" $ do
    it "is NaN when coerced to Number" $ do
      runJStr "console.log('1', +[1,2,3])" `shouldReturn` Right "1 NaN\n"
      runJStr "console.log('2', +(new Array(1,2,3)))" `shouldReturn` Right "2 NaN\n"

    it "is an instanceof Array" $ do
      jsEvalExpr "[] instanceof Array" `shouldReturn` VBool True

  describe "String object" $ do
    it "has charAt" $ do
      jsEvalExpr "new String('abc').charAt(2)" `shouldReturn` VStr "c"

  describe "the 'with' statement" $ do
    it "creates variables in the outer environment" $ do
      let prog = unlines [
                  "  try { var obj = { x: 9 }; ",
                  "  var x = 4; ",
                  "  with (obj) { ",
                  "    var x = 3; ",
                  "    throw x; ",
                  "  } } catch (e) { }",
                  "  console.log(x, obj.x); " ]
      runJStr prog `shouldReturn` Right "4 3\n"

  describe "Declaration Binding Initialization" $ do -- ref 10.5
    it "pre-declares variables" $ do
      runJStr "console.log(a)" `shouldError` "ReferenceError: No such variable a"
      runJStr "console.log(a); var a;" `shouldReturn` Right "undefined\n"
      runJStr "console.log(a); var a = 5;" `shouldReturn` Right "undefined\n"

    it "pre-declares function names" $ do
      runJStr "'use strict'; function a() { }" `shouldReturn` Right ""
