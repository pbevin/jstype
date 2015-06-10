module Builtins.Math where

import Control.Monad (liftM)
import Control.Arrow
import Runtime


mathObject :: Runtime (Shared JSObj)
mathObject = do
  newObject >>= addReadOnlyConstants mathConstants
            >>= addOwnProperty "abs"    (VNative 1 $ mathFunc abs)
            >>= addOwnProperty "log"    (VNative 1 $ mathFunc log)
            >>= addOwnProperty "exp"    (VNative 1 $ mathFunc exp)
            >>= addOwnProperty "sin"    (VNative 1 $ mathFunc sin)
            >>= addOwnProperty "cos"    (VNative 1 $ mathFunc cos)
            >>= addOwnProperty "tan"    (VNative 1 $ mathFunc tan)
            >>= addOwnProperty "asin"   (VNative 1 $ mathFunc asin)
            >>= addOwnProperty "acos"   (VNative 1 $ mathFunc acos)
            >>= addOwnProperty "atan"   (VNative 1 $ mathFunc atan)
            >>= addOwnProperty "sqrt"   (VNative 1 $ mathFunc sqrt)
            >>= addOwnProperty "ceil"   (VNative 1 $ roundish ceiling)
            >>= addOwnProperty "round"  (VNative 1 $ roundish jsRound)
            >>= addOwnProperty "floor"  (VNative 1 $ roundish floor)
            >>= addOwnProperty "trunc"  (VNative 1 $ roundish truncate)
            >>= addOwnProperty "random" (VNative 0 $ mathFunc $ const 4) -- xkcd #221

            >>= addOwnProperty "max"    (VNative 2 $ mathMaxFunc max $ -inf)
            >>= addOwnProperty "min"    (VNative 2 $ mathMaxFunc min    inf)

            >>= addOwnProperty "pow"    (VNative 2 $ mathFunc2 pow)
            >>= addOwnProperty "atan2"  (VNative 2 $ mathFunc2 atan2')
            >>= addOwnProperty "hypot"  (VNative 2 $ mathFunc2 hypot)

mathConstants :: [(String, JSNum)]
mathConstants = allToJSNum [ ("PI", pi),
                             ("SQRT2", sqrt 2),
                             ("SQRT1_2", sqrt 0.5),
                             ("E", exp 1),
                             ("LN2", log 2),
                             ("LN10", log 10),
                             ("LOG10E", 1 / log 10),
                             ("LOG2E", 1 / log 2) ]
  where allToJSNum = map (second JSNum)


inf :: Double
inf = 1/0

nan :: Double
nan = 0/0


atan2' :: Double -> Double -> Double
atan2' y x
  | y ==  1/0    &&    x ==  1/0    =     pi/4
  | y ==  1/0    &&    x == -1/0    =   3*pi/4
  | y == -1/0    &&    x ==  1/0    =    -pi/4
  | y == -1/0    &&    x == -1/0    =  -3*pi/4
  | otherwise                       = atan2 y x

roundish :: (Double -> Integer) -> JSFunction
roundish f = mathFunc g
  where g x = if isNaN x then nan else fromInteger $ f x

-- Haskell's round is different from Javascript's in its handling
-- of x.5 - Haskell rounds it to the nearest *even* number, and
-- JS rounds it to the next higher integer.
jsRound :: Double -> Integer
jsRound x = floor (x+0.5)


mathFunc :: (Double -> Double) -> JSFunction
mathFunc f _this args = liftM (VNum . JSNum . f . fromJSNum) $ toNumber (head args)

mathFunc2 :: (Double -> Double -> Double) -> JSFunction
mathFunc2 f _this args = do
  [x, y] <- mapM toNumber [a, b]
  return $ VNum $ JSNum $ f (fromJSNum x) (fromJSNum y)
    where (a, b) = case args of
                      []      -> (VNum 0, VNum 0)
                      [x]     -> (x, VNum 0)
                      (x:y:_) -> (x, y)

mathMaxFunc :: (JSNum -> JSNum -> JSNum) -> Double -> JSFunction
mathMaxFunc f ifEmpty _this args = do
  numbers <- mapM toNumber args
  return $ VNum $ foldl (handleNaN f) (JSNum ifEmpty) numbers
  where handleNaN :: (JSNum -> JSNum -> JSNum) -> JSNum -> JSNum -> JSNum
        handleNaN g a b = if isJsNaN a || isJsNaN b then JSNum nan else g a b
        isJsNaN (JSNum x) = x /= x

hypot :: Double -> Double -> Double
hypot a b = if isInfinite a || isInfinite b then inf else sqrt (a*a + b*b)

pow :: RealFloat a => a -> a -> a
pow x y
  | abs x == 1 && isInfinite y = 0/0
  | isNegativeZero x && y < 0 && isOddInteger y = -1/0
  | isNegativeZero x && y < 0 && not (isOddInteger y) = 1/0
  | x == 0 && y < 0 = 1/0
  | otherwise = x ** y
