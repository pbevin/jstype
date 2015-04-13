module ParseSpec where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Expr
import Parse
import Eval

spec = do
  it "parses a number" $ do
    parseExpr "1" `shouldBe` Num (JSNum 1)

  it "parses a negative number" $ do
    parseExpr "-1" `shouldBe` UnOp "-" (Num (JSNum 1))

  it "parses strings" $ do
    parseExpr "'single quoted'" `shouldBe` Str "single quoted"
    parseExpr "\"double quoted\"" `shouldBe` Str "double quoted"

  it "parses a unary operator" $ do
    parseExpr "++u" `shouldBe` UnOp "++" (ReadVar "u")

  it "parses a unop assignment" $ do
    parseExpr "e = -1" `shouldBe` Assign "e" "=" (UnOp "-" (Num (JSNum 1)))

  it "parses a plus-equals" $ do
    parseExpr "a += b" `shouldBe` Assign "a" "+=" (ReadVar "b")

  it "interprets this tricky case right" $ do
    parseExpr "a++ + ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
    parseExpr "a+++ ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
    evaluate (parseExpr "a+++++b") `shouldThrow` anyException
    evaluate (parseExpr "a++ +++b") `shouldThrow` anyException

  it "parses a unop assignment without spaces" $ do
    parseExpr "e=-1" `shouldBe` Assign "e" "=" (UnOp "-" (Num (JSNum 1)))

  it "parses a binop" $ do
    parseExpr "1+2" `shouldBe` BinOp "+" (Num (JSNum 1)) (Num (JSNum 2))
    parseExpr "(1)&&(2)" `shouldBe` BinOp "&&" (Num (JSNum 1)) (Num (JSNum 2))

  it "parses a chained binop" $ do
    parseExpr "a+b+c" `shouldBe` BinOp "+" (BinOp "+" (ReadVar "a") (ReadVar "b")) (ReadVar "c")

  it "parses a function call" $ do
    parseExpr "f()" `shouldBe` FunCall (ReadVar "f") []

  it "parses a function call with an argument" $ do
    parseExpr "f(x)" `shouldBe` FunCall (ReadVar "f") [ReadVar "x"]

  it "parses a function call with two arguments" $ do
    parseExpr "f(x,y)" `shouldBe` FunCall (ReadVar "f") [ReadVar "x", ReadVar "y"]

  it "parses a chained function call" $ do
    parseExpr "f()()" `shouldBe` FunCall (FunCall (ReadVar "f") []) []

  it "parses a while statement" $ do
    simpleParse "while (a) { }" `shouldBe` Program [WhileStatement (ReadVar "a") (Block [])]

  it "ignores comments at the start of the file" $ do
    simpleParse "" `shouldBe` Program []
    simpleParse "// this is a comment\n" `shouldBe` Program []
    simpleParse "// a\n//b\n2\n" `shouldBe` Program [ExpressionStatement (Num 2)]

  it "ignores white space before and after semicolons" $ do
    simpleParse "1 ; 2" `shouldBe` simpleParse "1;2"
    simpleParse "1; 2" `shouldBe` simpleParse "1;2"
    simpleParse "1 ;2" `shouldBe` simpleParse "1;2"

  it "parses a semicolon-terminated statement in a function" $ do
    simpleParse "function b() { return 3 }" `shouldBe` simpleParse "function b() { return 3; }"

  it "treats semicolons as optional" $ do
    simpleParse "a()\nb()\n" `shouldBe` simpleParse "a(); b();"
    -- XXX evaluate (simpleParse "a() b()") `shouldThrow` anyException

  it "is the inverse of showProg" $
    property prop_showProg
