module Builtins.Math where

import Control.Arrow
import Runtime


mathObject :: Runtime (Shared JSObj)
mathObject = do
  newObject >>= addReadOnlyConstants mathConstants
            >>= addOwnProperty "abs" (VNative $ mathFunc abs)
            >>= addOwnProperty "log" (VNative $ mathFunc log)
            >>= addOwnProperty "exp" (VNative $ mathFunc exp)
            >>= addOwnProperty "sin" (VNative $ mathFunc sin)
            >>= addOwnProperty "cos" (VNative $ mathFunc cos)
            >>= addOwnProperty "tan" (VNative $ mathFunc tan)
            >>= addOwnProperty "asin" (VNative $ mathFunc asin)
            >>= addOwnProperty "acos" (VNative $ mathFunc acos)
            >>= addOwnProperty "atan" (VNative $ mathFunc atan)
            >>= addOwnProperty "sqrt" (VNative $ mathFunc sqrt)
            >>= addOwnProperty "ceil" (VNative $ mathFunc $ fromIntegral . ceiling)
            >>= addOwnProperty "round" (VNative $ mathFunc $ fromIntegral . round)
            >>= addOwnProperty "floor" (VNative $ mathFunc $ fromIntegral . floor)
            >>= addOwnProperty "trunc" (VNative $ mathFunc $ fromIntegral . truncate)
            >>= addOwnProperty "random" (VNative $ mathFunc $ const 4) -- xkcd #221

            >>= addOwnProperty "max" (VNative $ mathMaxFunc maximum)
            >>= addOwnProperty "min" (VNative $ mathMaxFunc minimum)

            >>= addOwnProperty "pow" (VNative $ mathFunc2 pow)
            >>= addOwnProperty "atan2" (VNative $ mathFunc2 atan2')
            >>= addOwnProperty "hypot" (VNative $ mathFunc2 hypot)

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


atan2' :: Double -> Double -> Double
atan2' y x
  | y ==  1/0    &&    x ==  1/0    =     pi/4
  | y ==  1/0    &&    x == -1/0    =   3*pi/4
  | y == -1/0    &&    x ==  1/0    =    -pi/4
  | y == -1/0    &&    x == -1/0    =  -3*pi/4
  | otherwise                       = atan2 y x
