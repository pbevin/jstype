{-# LANGUAGE OverloadedStrings #-}

module Builtins.Math where

import Control.Monad (liftM)
import Control.Arrow
import Runtime
import Polyvariadic


mathObject :: Runtime (Shared JSObj)
mathObject = mkObject $ do
  className "Math"

  constant "PI"      $ (pi          :: Double)
  constant "SQRT2"   $ (sqrt 2      :: Double)
  constant "SQRT1_2" $ (sqrt 0.5    :: Double)
  constant "E"       $ (exp 1       :: Double)
  constant "LN2"     $ (log 2       :: Double)
  constant "LN10"    $ (log 10      :: Double)
  constant "LOG10E"  $ (1 / log 10  :: Double)
  constant "LOG2E"   $ (1 / log 2   :: Double)

  static "abs"    1 (abs  :: Double -> Double)
  static "log"    1 (log  :: Double -> Double)
  static "exp"    1 (exp  :: Double -> Double)
  static "sin"    1 (sin  :: Double -> Double)
  static "cos"    1 (cos  :: Double -> Double)
  static "tan"    1 (tan  :: Double -> Double)
  static "asin"   1 (asin :: Double -> Double)
  static "acos"   1 (acos :: Double -> Double)
  static "atan"   1 (atan :: Double -> Double)
  static "sqrt"   1 (sqrt :: Double -> Double)

  method "ceil"   1 (roundish ceiling)
  method "round"  1 (roundish jsRound)
  method "floor"  1 (roundish floor)
  method "trunc"  1 (roundish truncate)
  method "random" 0 (mathFunc $ const 4) -- xkcd #221

  method "max"    2 (mathMaxFunc max $ -jsInf)
  method "min"    2 (mathMaxFunc min    jsInf)

  method "pow"    2 (mathFunc2 pow)
  method "atan2"  2 (mathFunc2 atan2')
  method "hypot"  2 (mathFunc2 hypot)


atan2' :: Double -> Double -> Double
atan2' y x
  | y ==  1/0    &&    x ==  1/0    =     pi/4
  | y ==  1/0    &&    x == -1/0    =   3*pi/4
  | y == -1/0    &&    x ==  1/0    =    -pi/4
  | y == -1/0    &&    x == -1/0    =  -3*pi/4
  | otherwise                       = atan2 y x

roundish :: (Double -> Integer) -> JSFunction
roundish f = mathFunc g
  where g x = if isNaN x then jsNaN else fromInteger $ f x

-- Haskell's round is different from Javascript's in its handling
-- of x.5 - Haskell rounds it to the nearest *even* number, and
-- JS rounds it to the next higher integer.
jsRound :: Double -> Integer
jsRound x = floor (x+0.5)


mathFunc :: (Double -> Double) -> JSFunction
mathFunc f _this args = liftM (VNum . f) $ toNumber (head args)

mathFunc2 :: (Double -> Double -> Double) -> JSFunction
mathFunc2 f _this args = do
  [x, y] <- mapM toNumber [a, b]
  return $ VNum $ f x y
    where (a, b) = case args of
                      []      -> (VNum 0, VNum 0)
                      [x]     -> (x, VNum 0)
                      (x:y:_) -> (x, y)

mathMaxFunc :: (JSNum -> JSNum -> JSNum) -> Double -> JSFunction
mathMaxFunc f ifEmpty _this args = do
  numbers <- mapM toNumber args
  return $ VNum $ foldl (handleNaN f) ifEmpty numbers
  where handleNaN :: (JSNum -> JSNum -> JSNum) -> JSNum -> JSNum -> JSNum
        handleNaN g a b = if isNaN a || isNaN b then jsNaN else g a b

hypot :: Double -> Double -> Double
hypot a b = if isInfinite a || isInfinite b then jsInf else sqrt (a*a + b*b)

pow :: RealFloat a => a -> a -> a
pow x y
  | abs x == 1 && isInfinite y = 0/0
  | isNegativeZero x && y < 0 && isOddInteger y = -1/0
  | isNegativeZero x && y < 0 && not (isOddInteger y) = 1/0
  | x == 0 && y < 0 = 1/0
  | otherwise = x ** y
