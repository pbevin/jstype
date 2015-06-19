module Runtime.NumberToString where

import Control.Exception
import Data.Char
import Debug.Trace
import Prelude hiding (exponent)

type Base = Integer
type Precision = Int
type Exponent = Int
type Epsilon = Double

-- ref 9.8.1
numberToString :: Double -> String
numberToString m
  | isNaN m   = "NaN"
  | m == 1/0  = "Infinity"
  | m == 0    = "0"
  | m < 0     = "-" ++ numberToString (negate m)
  | otherwise = n2s m

n2s :: Double -> String
n2s f
  | k <= n && n <= 21   = d ++ replicate (n-k) '0'
  | 0 <  n && n <= 21   = take n d ++ "." ++ drop n d
  | -6 < n && n <= 0    = "0." ++ replicate (-n) '0' ++ d
  | k == 1              = d ++ "e" ++ exponent
  | otherwise           = take 1 d ++ "." ++ drop 1 d ++ "e" ++ exponent
  where (n, k, s) = splitNumber f
        d = map (intToDigit . fromIntegral) s
        exponent
          | n-1 > 0   = "+" ++ show (n-1)
          | otherwise = "-" ++ show (1-n)

type FPPState = (Int, Int, Integer, Integer, Integer, Integer)

dissectFloat :: RealFloat a => a -> (Base, Exponent, Integer, Precision)
dissectFloat d = (b, e, f, p)
  where b = floatRadix d
        e = e' + p
        p = floatDigits d
        (f, e') = decodeFloat d

splitNumber :: Double -> (Int, Int, [Integer])
splitNumber v = fpp2 . fixup $ start
  where
      start :: FPPState
      start = if f == shiftb 1 (p-1)
              then (0, 0, r*b, s*b, mm, mp*b)
              else (0, 0, r, s, mm, mp)

        where
          (b, e, f, p) = dissectFloat v
          r  = shiftb f $ max 0 $ e-p
          s  = shiftb 1 $ max 0 $ p-e
          mm = shiftb 1 $ max 0 $ e-p
          mp = mm

          shiftb :: Integer -> Int -> Integer
          shiftb x n = foldr (.) id (replicate n (*b)) x

      fixup :: FPPState -> FPPState
      fixup (n,k,r,s,mm,mp) =
        if 10*r < s + 10
        then fixup (n-1, k, r*10, s, mm*10, mp*10)
        else fixup' (n,k,r,s,mm,mp)

      fixup' :: FPPState -> FPPState
      fixup' (n,k,r,s,mm,mp) =
        if 2*r + mp >= 2*s
        then fixup' (n+1, k, r, s*10, mm, mp)
        else (n,k,r,s,mm,mp)

      fpp2 :: FPPState -> (Int, Int, [Integer])
      fpp2 (n,k,r,s,mm,mp) =
        let (u, r') = (10*r) `divMod` s
            mm' = 10 * mm
            mp' = 10 * mp
            low = 2*r' < mm'
            high = 2*r' + mp' >= 2*s
        in if not low && not high
           then let (n', k', s') = fpp2 (n, k+1, r', s, mm', mp')
                in (n', k', u : s')
           else (n, k+1, [ finalize low high u r' s ])

      finalize :: Bool -> Bool -> Integer -> Integer -> Integer -> Integer
      finalize low high u r s
        | low && not high = u
        | high && not low = u+1
        | 2*r < s         = u
        | otherwise       = u+1


