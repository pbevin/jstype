{-# LANGUAGE OverloadedStrings #-}
module CompilerSpec where

import Test.Hspec
import Compiler
import Runtime
import Expr

spec :: Spec
spec = do
  describe "compileObjectLiteral" $ do
    it "compiles an empty object" $ do
      compileObjectLiteral [] `shouldBe` [ OpObjLit 0 ]

    it "compiles {x:0}" $ do
      straighten (compileObjectLiteral [("x", Value (Num 0))]) `shouldBe`
        [ OpConst (VStr "x"), OpConst (VNum 0), OpGetValue, OpObjLit 1 ]
