{-# LANGUAGE LambdaCase #-}

module EvalSpec where

import Test.Hspec

import Expr
import Eval
import Runtime
import Expectations



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
    runJStr "Object.prototype.x = 1; console.log(Object.prototype.x)" `shouldReturn` Right "1\n"

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
      runJStr "'use strict'; var eval = 42;" `shouldErrorOfType` SyntaxError

    it "will not assign to a var called 'arguments'" $ do
      runJStr "'use strict'; var arguments = 42;" `shouldErrorOfType` SyntaxError

    it "applies to an eval in a strict function" $ do
      let prog = unlines [ "function f() {",
                           "  'use strict';",
                           "  eval('var public = 1; console.log(\"no\");');",
                           "}",
                           "f();" ]
      -- XXX The location is wrong - s/b "column 5"
      runJStr prog `shouldError` "SyntaxError: \"(eval)\" (line 1, column 12):\nunexpected reserved word \"public\"\nexpecting var declaration"

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

  describe "++ and --" $ do
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

    it "converts to numeric" $ do
      runJStr "var a = '5'; console.log(typeof a++, typeof a)"
        `shouldReturn` Right "number number\n"

  it "evaluates a primitive number wrapped in an object" $ do
    jsEvalExpr "{valueOf: function() {return 1}} + 1" `shouldReturn` VNum 2

  it "evaluates date objects specially" $ do
    runJStr "var date = new Date(); console.log(date + date == date.toString() + date.toString())"
      `shouldReturn` Right "true\n"

  it "can add a number and an object" $ do
    runJStr "console.log(1 + new Object())" `shouldReturn` Right "1[object Object]\n"

  describe "Object literal" $ do
    it "creates properties" $ do
      jsEvalExpr "{ a: 1, 2: 3 }['a']" `shouldReturn` VNum 1
      jsEvalExpr "{ a: 1, 2: 3 }['2']" `shouldReturn` VNum 3

    it "can have a getter" $ do
      let prog = unlines [
                  "  var obj = { ",
                  "    get x() { return 1; } ",
                  "  }; ",
                  "  console.log(obj.x); " ]
      runJStr prog `shouldReturn` Right "1\n"

    it "can have a setter" $ do
      let prog = unlines [
                  "  var v = 2; ",
                  "  var obj = { ",
                  "    get foo() { ",
                  "      return v + 1; ",
                  "    }, ",
                  "    set foo(x) { ",
                  "      v = x - 1; ",
                  "    } ",
                  "  } ",
                  "  console.log(obj.foo); ",
                  "  obj.foo = 42; ",
                  "  console.log(obj.foo); ",
                  "  console.log(v); " ]
      runJStr prog `shouldReturn` Right "3\n42\n41\n"



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

  describe "[a,b,,d,e]" $ do
    it "has a at [0]" $ do
      jsEvalExpr "['a','b',,'d','e'][0]" `shouldReturn` VStr "a"
    it "has b at [1]" $ do
      jsEvalExpr "['a','b',,'d','e'][1]" `shouldReturn` VStr "b"
    it "has undefined at [2]" $ do
      jsEvalExpr "['a','b',,'d','e'][2]" `shouldReturn` VUndef
    it "has d at [3]" $ do
      jsEvalExpr "['a','b',,'d','e'][3]" `shouldReturn` VStr "d"
    it "has e at [4]" $ do
      jsEvalExpr "['a','b',,'d','e'][4]" `shouldReturn` VStr "e"

  describe "printing an array" $ do
    it "prints the elements joined by comma" $ do
      runJStr "console.log(new Array(1,2,3).toString())" `shouldReturn` Right "1,2,3\n"
      runJStr "console.log([4,5,6].toString())" `shouldReturn` Right "4,5,6\n"

  describe "comparison" $ do
    it "understands <" $ do
      jsEvalExpr "1 < 2" `shouldReturn` VBool True
      jsEvalExpr "2 < 1" `shouldReturn` VBool False
      jsEvalExpr "1 < 1" `shouldReturn` VBool False

    it "understands <=" $ do
      jsEvalExpr "1 <= 2" `shouldReturn` VBool True
      jsEvalExpr "2 <= 1" `shouldReturn` VBool False
      jsEvalExpr "1 <= 1" `shouldReturn` VBool True

    it "understands <= edge cases" $ do
      jsEvalExpr "null <= undefined" `shouldReturn` VBool False

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

  describe "&&" $ do
    it "is true if both sides are true" $ do
      jsEvalExpr "true && true" `shouldReturn` VBool True

    it "is false if either side is false" $ do
      jsEvalExpr "false && true"  `shouldReturn` VBool False
      jsEvalExpr "true  && false" `shouldReturn` VBool False
      jsEvalExpr "false && false" `shouldReturn` VBool False

    it "does not evaluate the RHS if the LHS is false" $ do
      jsEvalExpr "x=1, (false && x++), x" `shouldReturn` VNum 1

  describe "||" $ do
    it "is false if both sides are false" $ do
      jsEvalExpr "false || false" `shouldReturn` VBool False

    it "is false if either side is false" $ do
      jsEvalExpr "true  || false" `shouldReturn` VBool True
      jsEvalExpr "false || true " `shouldReturn` VBool True
      jsEvalExpr "true  || true " `shouldReturn` VBool True

    it "does not evaluate the RHS if the LHS is false" $ do
      jsEvalExpr "x=1, (false && x++), x" `shouldReturn` VNum 1

  it "does bitshift operations" $ do
    jsEvalExpr "3 << 2" `shouldReturn` VNum 12
    jsEvalExpr "12 >> 2" `shouldReturn` VNum 3

  it "does bitwise not" $ do
    jsEvalExpr "~0" `shouldReturn` VNum (-1)

  describe "left shift" $ do
    it "does hard case 5" $ do
      jsEvalExpr "6442450943.1 << 0" `shouldReturn` VNum 2147483647
    it "does hard case 6" $ do
      jsEvalExpr "-2147483649.1 << 0" `shouldReturn` VNum 2147483647

  it "runs loops" $ do
    runJStr "var t = 0, i; for (i = 0; i < 10; i++) { t += i }; console.log(t);" `shouldReturn` Right "45\n"

  it "can break out of a while loop" $ do
    runJStr "var i = 0; while (true) { if (i++ > 10) break; }; console.log(i);" `shouldReturn` Right "12\n"

  it "can break out of a while loop in a function" $ do
    runJStr "var a = 'yes'; (function(){ do { break; a = 'no';} while (0); })();console.log(a);"
         `shouldReturn` Right "yes\n"

  it "runs do-while loops" $ do
    runJStr "var t = 3; do { console.log(t--) } while (t > 0);"
      `shouldReturn` Right "3\n2\n1\n"

  it "can break out of an outer loop" $ do
    runJStr "var x = 1; L1: do { L2: do { x++; break L1; x++ } while (0); x++; } while (0); console.log(x);"
      `shouldReturn` Right "2\n"

  it "can break out of a for loop from a catch block" $ do
    jsEvalExpr "(function(x){FOR : for(;;){ try{ x++; throw 1; } catch(e){ break FOR; }}; return x })(0)"
      `shouldReturn` VNum 1

  it "does not attach a break label after a newline" $ do
    jsEvalExpr "(function() {FOR1 : for(var i=1;i<2;i++){ LABEL1 : do {var x =1;break\nFOR1;var y=2;} while(0);} return i;})()"
      `shouldReturn` VNum 2

  it "runs a for..var loop" $ do
    runJStr "for (var i = 0, t = 0; i < 10; i++, t+=i); console.log(t);" `shouldReturn` Right "55\n"

  it "runs a for..var..in loop" $ do
    runJStr "var a = ['apple', 'box', 'car']; for (var n in a) { console.log(a[n]); }" `shouldReturn` Right "apple\nbox\ncar\n"

  it "can call a function" $ do
    runJStr "function print(msg) { console.log(msg); }; print(\"hi\")" `shouldReturn` Right "hi\n"

  it "can set an object property" $ do
    runJStr "var a = function() { }; a.prop = 2; console.log(a.prop);" `shouldReturn` Right "2\n"

  it "returns undefined when the function does not explicitly return" $ do
    jsEvalExpr "(function() { x = 1; })()" `shouldReturn` VUndef

  it "has a nice error message when accessing a property on null or undefined" $ do
    runJStr "undefined.x" `shouldError` "TypeError: Cannot read property x of undefined"
    runJStr "null.x" `shouldError` "TypeError: Cannot read property x of null"

  it "can do if-then-else" $ do
    runJStr "if (1) { console.log(\"hi\") }" `shouldReturn` Right "hi\n"
    runJStr "if (0) { console.log(\"wrong\") } else { console.log(\"yes\") } " `shouldReturn` Right "yes\n"

  it "can do typeof" $ do
    runJStr "console.log(typeof console)" `shouldReturn` Right "object\n"
    runJStr "console.log(typeof this)" `shouldReturn` Right "object\n"
    runJStr "console.log(typeof 5)" `shouldReturn` Right "number\n"
    runJStr "console.log(typeof 'aa')" `shouldReturn` Right "string\n"
    runJStr "console.log(typeof Object.toString)" `shouldReturn` Right "function\n"

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
    -- S11.2.1_A3_T1
    jsEvalExpr "true.toString()" `shouldReturn` VStr "true"

  it "can get a character in a string with []" $ do
    jsEvalExpr "'abc'[1]" `shouldReturn` VStr "b"

  it "can define a simple object" $ do
    let prog = unlines [
          "function Counter(v) { this.val = v; };",
          "Counter.prototype.inc = function() { this.val++ };",
          "var counter = new Counter(5);",
          "counter.inc();",
          "counter.inc();",
          "console.log(counter.val);" ]

    runJStr prog `shouldReturn` Right "7\n"

  it "can create a new object" $ do
    runJStr "var x = new Object(); console.log(x)" `shouldReturn` Right "[object Object]\n"

  it "runs a try..catch block" $ do
    runJStr "try { throw new Error('hi') } catch (e) { console.log(e.message); }"
    `shouldReturn` Right "hi\n";

  it "encapsulates the catch block arg in its own lexical environment" $ do
    let prog = unlines [
                  "  function captured() {return e}; ",
                  "  e = \"prior to throw\"; ",
                  "  try { ",
                  "    throw new Error(); ",
                  "  } ",
                  "  catch (e) { ",
                  "    var e = \"initializer in catch\"; ",
                  "    console.log(captured()); ",
                  "  } "
                ]

    runJStr prog `shouldReturn` Right "prior to throw\n"

  it "can catch a reference error" $ do
    jsEvalExpr "(function f() {try { return x; } catch(e) { return 71; }})()" `shouldReturn` VNum 71

  it "raises an error to the top level" $ do
    runJStr "throw 'hi'" `shouldError` "hi"

  it "evaluates || properly" $ do
    runJStr "console.log(1 || 2)" `shouldReturn` Right "1\n"
    runJStr "console.log(false || 2)" `shouldReturn` Right "2\n"

  it "raises runtime exceptions" $ do
    runJStr "var a; a();" `shouldError` "TypeError: a is undefined"

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

    it "uses a different context when called indirectly" $ do
      runJStr "var a = 1; function f() { var eval1 = eval, a = 2; console.log(eval('a'), eval1('a')) }; f();"
        `shouldReturn` Right "2 1\n"

  describe "Boolean" $ do
    it "can be called as a function" $ do
      jsEvalExpr "Boolean(true)" `shouldReturn` VBool True

    it "can be called as a constructor" $ do
      v <- jsEvalExpr "new Boolean(true)"
      v `shouldSatisfy` isObj

    it "returns a boolean from valueOf()" $ do
      jsEvalExpr "new Boolean(true).valueOf()" `shouldReturn` VBool True

    it "can act as a number" $ do
      jsEvalExpr "new Boolean(true) + 1" `shouldReturn` VNum 2

  describe "Directive Prologue" $ do
    it "is evaluated" $
      runJStr "console.log(eval('\"use strict\"'))" `shouldReturn` Right "use strict\n"


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

    it "can return a variable value from a getter" $ do
      runJStr "o={get x(){var n=42;return n}}; console.log(o.x);"
        `shouldReturn` Right "42\n"

  describe "Object constructor" $ do
    it "calls toObject() on primitive values" $ do
      jsEvalExpr "new Object(42).toString()" `shouldReturn` VStr "42"
      jsEvalExpr "new Object(42).valueOf()" `shouldReturn` VNum 42

  describe "Array" $ do
    it "is NaN when coerced to Number" $ do
      runJStr "console.log('1', +[1,2,3])" `shouldReturn` Right "1 NaN\n"
      runJStr "console.log('2', +(new Array(1,2,3)))" `shouldReturn` Right "2 NaN\n"

    it "is an instanceof Array" $ do
      jsEvalExpr "[] instanceof Array" `shouldReturn` VBool True

    it "has constructor Array" $ do
      jsEvalExpr "[].constructor.name" `shouldReturn` VStr "Array"

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

    it "can delete a property from its own getter" $ do
      let prog = unlines [
                  "  var x = 0; ",
                  "  var scope = { ",
                  "    get x() { ",
                  "      delete this.x; ",
                  "      return 2; ",
                  "    } ",
                  "  }; ",
                  "  with (scope) { ",
                  "    x--; ",
                  "  } ",
                  "  console.log(x, scope.x)" ]
      runJStr prog `shouldReturn` Right "0 1\n"

    it "gives functions access to its scope" $ do
      let prog = unlines [
                  "  var a = 1; ",
                  "  var obj = {a:2}; ",
                  "  with (obj) { ",
                  "      (function(){console.log(a);})(); ",
                  "  } " ]
      runJStr prog `shouldReturn` Right "2\n"

    it "can override a constant" $ do
      runJStr "with ({ Infinity: 3 }) { console.log(Infinity) }"
        `shouldReturn` Right "3\n"


  describe "delete" $ do
    it "deletes an undeclared variable" $ do
      runJStr "x = 1; delete x; console.log(x);"
        `shouldError` "ReferenceError: No such variable x"

    it "will not delete a declared variable" $ do
      runJStr "var x = 1; delete x; console.log(x)"
        `shouldReturn` Right "1\n"


  describe "Declaration Binding Initialization" $ do -- ref 10.5
    it "pre-declares variables" $ do
      runJStr "console.log(a)" `shouldError` "ReferenceError: No such variable a"
      runJStr "console.log(a); var a;" `shouldReturn` Right "undefined\n"
      runJStr "console.log(a); var a = 5;" `shouldReturn` Right "undefined\n"

    it "pre-declares function names" $ do
      runJStr "'use strict'; function a() { }" `shouldReturn` Right ""

  describe "instanceof" $ do
    it "is true for objects of the same type" $ do
      jsEvalExpr "new Number instanceof Number" `shouldReturn` VBool True
      jsEvalExpr "new Error instanceof Error" `shouldReturn` VBool True
      jsEvalExpr "new TypeError instanceof TypeError" `shouldReturn` VBool True

    it "is true for a subclass instance (typeError instanceof Error)" $ do
      jsEvalExpr "new TypeError instanceof Error" `shouldReturn` VBool True

    it "is true for a raised error" $ do
      runJStr "try { ({}) instanceof this } catch (e) { if (e instanceof TypeError) console.log('ok') }" `shouldReturn` Right "ok\n"

    it "is false for unrelated types" $ do
      jsEvalExpr "new Error instanceof Number" `shouldReturn` VBool False

    it "is false for superclass instances" $ do
      jsEvalExpr "new Error instanceof TypeError" `shouldReturn` VBool False

    it "is a type error when RHS is not a function object" $ do
      runJStr "({}) instanceof this" `shouldError` "TypeError: Expecting a function in instanceof"

  describe "Property assignment" $ do
    it "silently refuses to assign to Math.E" $ do
      -- language/expressions/assignment/S8.12.4_A1
      runJStr "Math.E = 1; console.log(Math.E)" `shouldReturn` Right "2.718281828459045\n"

    it "throws a type exception whe assigning to Math.E in strict mode" $ do
      -- language/expressions/assignment/11.13.1-4-28gs
      runJStr "'use strict'; Math.E = 1; console.log(Math.E)" `shouldError`
        "TypeError: Attempt to overwrite read-only property E"

    it "refuses to assign to a prototype property" $ do
      runJStr "function x() {}; Object.defineProperty(x.prototype, 'y', {value:33, writable: false}); xx = new x(); xx.y = 4; console.log(xx.y)" `shouldReturn` Right "33\n"


  describe "Function handling" $ do
    it "can invoke a function defined later" $
      runJStr "console.log(f()); function f() { return 'nice' }"
        `shouldReturn` Right "nice\n"

    it "can invoke an inner function defined later" $
      runJStr "g(); function g() { console.log(f()); function f() { return 'inner' } }"
        `shouldReturn` Right "inner\n"

    it "can call a function object" $ do
      runJStr "var f = new Function(\"console.log('yes');\"); f()"
        `shouldReturn` Right "yes\n"

    it "can call a function object via its .call()" $ do
      runJStr "var f = new Function(\"console.log('called');\"); f.call()"
        `shouldReturn` Right "called\n"

    it "raises a syntax error when it cannot parse a new Function(..) body" $ do
      runJStr "var f = new Function(' ', '\"use strict\"; eval = 4'); f();" `shouldErrorOfType` SyntaxError

    it "can invoke a function defined later in eval" $
      runJStr "eval('console.log(h()); function h() { return \"ok\" }')"
        `shouldReturn` Right "ok\n"

    it "supplies .length for a native function" $ do
      jsEvalExpr "Object.defineProperty.length" `shouldReturn` (VNum 3)

    it "handles arguments.constructor properly" $ do
      -- language/arguments-object/S10.6_A2
      jsEvalExpr "(function() { return arguments.constructor.prototype; })() === Object.prototype" `shouldReturn` VBool True

    it "sets unpassed params to undefined" $ do
      runJStr "function f(a) { console.log(a); }; f();"
        `shouldReturn` Right "undefined\n"

  describe "scope" $ do
    it "allows access to the outer scope in a catch block" $ do
      let prog = unlines [
                    "  function f(a, x) { ",
                    "    try { ",
                    "      throw new Error(); ",
                    "    } catch (e) { ",
                    "      console.log(x + 3); ",
                    "    } ",
                    "  } ",
                    "  new f('a', 5); "
                  ]

      runJStr prog `shouldReturn` Right "8\n"

    it "brings back scope after an exception in a function" $ do
      let prog = unlines [
                "  function err() { throw 'aa'; } ",
                "  function f(x) { ",
                "    try{ ",
                "      err(); ",
                "    } ",
                "    catch (e) { ",
                "      console.log(x); ",
                "    } ",
                "  } ",
                "  f(13); " ]

      runJStr prog `shouldReturn` Right "13\n"

  describe "the switch statement" $ do
    -- 12.11_A1_T1
    let prog = unlines [
                "  function test(value){ ",
                "    var result = 0; ",
                "    switch(value) { ",
                "      case 0:  result += 2; ",
                "      case 1:  result += 4; break; ",
                "      case 2:  result += 8; ",
                "      case 3:  result += 16; ",
                "      default: result += 32; break; ",
                "      case 4:  result += 64; ",
                "    } ",
                "    console.log(result); ",
                "  }; " ]
    let test val = runJStr (prog ++ "test(" ++ val ++ ");")

    specify "case 0" $ test "0" `shouldReturn` Right "6\n"
    specify "case 1" $ test "1" `shouldReturn` Right "4\n"
    specify "case 2" $ test "2" `shouldReturn` Right "56\n"
    specify "case 3" $ test "3" `shouldReturn` Right "48\n"
    specify "case 4" $ test "4" `shouldReturn` Right "64\n"
    specify "non-match 1" $ test "true"   `shouldReturn` Right "32\n"
    specify "non-match 2" $ test "false"  `shouldReturn` Right "32\n"
    specify "non-match 3" $ test "null"   `shouldReturn` Right "32\n"
    specify "non-match 4" $ test "void 0" `shouldReturn` Right "32\n"
    specify "non-match 5" $ test "\"0\""  `shouldReturn` Right "32\n"
