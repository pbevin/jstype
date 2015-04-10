module ParseSpec where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Expr
import Parse

spec = do
  it "parses a number" $ do
    parseExpr "1" `shouldBe` Num (JSNum 1)

  it "parses a negative number" $ do
    parseExpr "-1" `shouldBe` UnOp "-" (Num (JSNum 1))

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

  it "is the inverse of showProg" $
    property prop_showProg
