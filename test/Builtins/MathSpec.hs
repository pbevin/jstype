module Builtins.MathSpec where

import Test.Hspec
import Runtime
import Eval
import Builtins.Math


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
      atan2'   17       inf      `shouldBe`      0
      atan2'   17       (-inf)   `shouldBe`      pi
      atan2' (-17)      inf      `shouldBe`     -0
      atan2' (-17)      (-inf)   `shouldBe`     -pi
      atan2'   inf      17       `shouldBe`      pi/2
      atan2'   (-inf)   17       `shouldBe`     -pi/2
      atan2'   inf      inf      `shouldBe`      pi/4
      atan2'   inf      (-inf)   `shouldBe`    3*pi/4
      atan2'   (-inf)   inf      `shouldBe`     -pi/4
      atan2'   (-inf)   (-inf)   `shouldBe`   -3*pi/4

  describe "round" $ do
    it "rounds x.5 to the next higher integer" $ do
      jsEvalExpr "Math.round(0.5)" `shouldReturn` VNum 1.0
      jsEvalExpr "Math.round(-1.5)" `shouldReturn` VNum (-1.0)

