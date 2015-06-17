{-# LANGUAGE OverloadedStrings #-}

module Builtins.StringSpec where

import Test.Hspec
import Builtins.String


spec :: Spec
spec = do
  describe "findOnce" $ do
    it "splits the string on the first occurrence" $ do
      findOnce "+" "a+b+c" `shouldBe` Just ("a", "+", "b+c")

    it "returns Nothing if the string is not found" $ do
      findOnce "*" "a+b+c" `shouldBe` Nothing
