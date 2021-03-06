{-# LANGUAGE OverloadedStrings #-}

module Builtins.MathSpec where

import Test.Hspec
import Runtime
import Eval
import Builtins.Math
import JSNum


spec :: Spec
spec = do
  describe "atan2" $ do
    -- If y>0 and y is finite and x is +∞, the result is +0.
    -- If y>0 and y is finite and x is −∞, the result if an approximation to +π.
    -- If y<0 and y is finite and x is +∞, the result is −0.
    -- If y<0 and y is finite and x is −∞, the result is an approximation to −π.
    -- If y is +∞ and x is finite, the result is an approximation to +π/2.
    -- If y is −∞ and x is finite, the result is an approximation to −π/2.
    -- If y is +∞ and x is +∞, the result is an approximation to +π/4.
    -- If y is +∞ and x is −∞, the result is an approximation to +3π/4.
    -- If y is −∞ and x is +∞, the result is an approximation to −π/4.
    -- If y is −∞ and x is −∞, the result is an approximation to −3π/4.
    it "follows the infinity rules in 15.8.2.5" $ do
      atan2'   17         jsInf      `shouldBe`      0
      atan2'   17       (-jsInf)     `shouldBe`      pi
      atan2' (-17)        jsInf      `shouldBe`     -0
      atan2' (-17)      (-jsInf)     `shouldBe`     -pi
      atan2'   jsInf      17         `shouldBe`      pi/2
      atan2'   (-jsInf)   17         `shouldBe`     -pi/2
      atan2'   jsInf      jsInf      `shouldBe`      pi/4
      atan2'   jsInf      (-jsInf)   `shouldBe`    3*pi/4
      atan2'   (-jsInf)   jsInf      `shouldBe`     -pi/4
      atan2'   (-jsInf)   (-jsInf)   `shouldBe`   -3*pi/4

  describe "round" $ do
    it "rounds x.5 to the next higher integer" $ do
      jsEvalExpr "Math.round(0.5)" `shouldReturn` VNum 1.0
      jsEvalExpr "Math.round(-1.5)" `shouldReturn` VNum (-1.0)

  describe "min, max" $ do
    it "can get the min/max of a nonempty list" $ do
      jsEvalExpr "Math.max(1,3,2)" `shouldReturn` VNum 3
      jsEvalExpr "Math.min(1,3,2)" `shouldReturn` VNum 1

    it "defaults the empty list" $ do
      jsEvalExpr "Math.max()" `shouldReturn` VNum (-1/0)
      jsEvalExpr "Math.min()" `shouldReturn` VNum ( 1/0)

  describe "hypot" $ do
    it "is the hypotenuse of a right triangle" $ do
      jsEvalExpr "Math.hypot(3, 4)" `shouldReturn` VNum 5

    it "returns infinity if either arg is infinite" $ do
      jsEvalExpr "Math.hypot( Infinity, 3)" `shouldReturn` VNum jsInf
      jsEvalExpr "Math.hypot(-Infinity, 3)" `shouldReturn` VNum jsInf
      jsEvalExpr "Math.hypot(3,  Infinity)" `shouldReturn` VNum jsInf
      jsEvalExpr "Math.hypot(3, -Infinity)" `shouldReturn` VNum jsInf

    it "is 0 if no arg is given" $ do
      jsEvalExpr "Math.hypot()" `shouldReturn` VNum 0

  describe "Math.pow" $ do
    it "raises one number to the power of another" $ do
      pow 3 2 `shouldBe` 9

    it "is NaN for +/-1 and +/-Infinity" $ do
      pow 1 (-jsInf) `shouldSatisfy` isNaN
      pow (-1) (-jsInf) `shouldSatisfy` isNaN
      pow 1 jsInf `shouldSatisfy` isNaN
      pow (-1) jsInf `shouldSatisfy` isNaN

    it "is +Infinity when x is +0 and y < 0" $ do
      pow 0 (-1) `shouldBe` jsInf
      pow 0 (-2) `shouldBe` jsInf

    it "is -Infinity when x is -0, y < 0, and y is an odd integer" $ do
      -- yes indeed
      pow (-0) (-11111) `shouldBe` -jsInf

    it "is +Infinity when x is -0, y < 0, and y not an odd integer" $ do
      pow (-0) (-2) `shouldBe` jsInf
      pow (-0) (-jsInf) `shouldBe` jsInf
      pow (-0) (-1.79e308) `shouldBe` jsInf

