{-# LANGUAGE OverloadedStrings #-}

module Builtins.RegExpSpec where

import Test.Hspec
import Builtins
import Runtime
import Eval

spec :: Spec
spec = do
  describe "RegExp constructor" $ do
    it "copies pattern and flags from an existing regexp" $ do
      jsEvalExpr "new RegExp(/abc/g).source" `shouldReturn` VStr "abc"
      jsEvalExpr "new RegExp(/abc/g).global" `shouldReturn` VBool True
      jsEvalExpr "new RegExp(/abc/g).multiline" `shouldReturn` VBool False
      jsEvalExpr "new RegExp(/abc/g).ignoreCase" `shouldReturn` VBool False
      jsEvalExpr "new RegExp(/abc/g).toString()" `shouldReturn` VStr "/abc/g"

    it "sets the source" $ do
      jsEvalExpr "/abc/.source" `shouldReturn` VStr "abc"
      jsEvalExpr "/(.*)/.source" `shouldReturn` VStr "(.*)"

    it "sets the global flag" $ do
      jsEvalExpr "/abc/.global" `shouldReturn` VBool False
      jsEvalExpr "/abc/g.global" `shouldReturn` VBool True

    it "sets the multiline flag" $ do
      jsEvalExpr "/abc/.multiline" `shouldReturn` VBool False
      jsEvalExpr "/abc/m.multiline" `shouldReturn` VBool True

    it "sets the ignoreCase flag" $ do
      jsEvalExpr "/abc/.ignoreCase" `shouldReturn` VBool False
      jsEvalExpr "/abc/i.ignoreCase" `shouldReturn` VBool True

    it "returns the flags in toString()" $ do
      jsEvalExpr "/abc/mgi.toString()" `shouldReturn` VStr "/abc/gim"
      jsEvalExpr "/abc/g.toString()" `shouldReturn` VStr "/abc/g"
