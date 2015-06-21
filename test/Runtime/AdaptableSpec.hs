module Runtime.AdaptableSpec where

import Test.Hspec
import Runtime



runJS :: Runtime a -> IO a
runJS a = runRuntime a >>= \((Right r, _), _) -> return r



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
    runJS (f VUndef [VNum 3]) `shouldReturn` VNum 9

  it "can run a function of 2 args and no `this`" $ do
    let f = adapt hyp
    runJS (f VUndef [VNum 3, VNum 4]) `shouldReturn` VNum 5

  it "can run a String->Int function" $ do
    let f = adapt strlen
    runJS (f VUndef [VStr "hello world"]) `shouldReturn` VNum 11
