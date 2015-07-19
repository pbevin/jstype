module Runtime.AdaptableSpec where

import Test.Hspec
import Expectations
import Runtime





sq :: Double -> Double
sq x = x*x

hyp :: Double -> Double -> Double
hyp x y = sqrt (sq x + sq y)

strlen :: String -> Int
strlen = length


spec :: Spec
spec = do
  it "can run a function of 1 arg and no `this`" $ do
    let f = adapt sq
    run (f VUndef [VNum 3]) `shouldReturn` VNum 9

  it "can run a function of 2 args and no `this`" $ do
    let f = adapt hyp
    run (f VUndef [VNum 3, VNum 4]) `shouldReturn` VNum 5

  it "can run a String->Int function" $ do
    let f = adapt strlen
    run (f VUndef [VStr "hello world"]) `shouldReturn` VInt 11
