module Runtime.NumberToString where

-- ref 9.8.1
numberToString :: Double -> String
numberToString m
  | isNaN m   = "NaN"
  | m == 1/0  = "Infinity"
  | m == 0    = "0"
  | m < 0     = "-" ++ numberToString (negate m)
  | otherwise =
    let (_, n, k, s) = splitNumber m
    in numberToString' n k s

numberToString' :: Int -> Int -> Integer -> String
numberToString' n k s
  | k <= n && n <= 21   = d ++ replicate (n-k) '0'
  | 0 <  n && n <= 21   = take n d ++ "." ++ drop n d
  | -6 < n && n <= 0    = "0." ++ replicate (-n) '0' ++ show s
  | k == 1              = d ++ "e" ++ exponent n
  | otherwise           = take 1 d ++ "." ++ drop 1 d ++ "e" ++ exponent n
  where d = show s
        exponent n
          | n-1 > 0 = "+" ++ show (n-1)
          | n-1 < 0 = "-" ++ show (1-n)

splitNumber :: Double -> (Double, Int, Int, Integer)
splitNumber m = (m, n, k, s)
  where
    k = findk m
    (n, s) = findn m k

findk :: Double -> Int
findk d = fst $ findk' (1, d)
  where
    findk' :: (Int, Double) -> (Int, Integer)
    findk' (k, d) = findk'' (k, d)
    findk'' (k, d)
      | d < 1 = findk' (k, d*10)
      | d >= 10 = findk' (k, d/10)
      | otherwise =
          let (a, b) = properFraction d
          in if b / epsilon < 1 || 1-b < epsilon || k >= 16 then (k, a) else findk' (k+1, b * 10)
    epsilon = 1.0 / 10^8


findn :: Double -> Int -> (Int, Integer)
findn d k = findn' (k, k, d)
  where
    findn' :: (Int, Int, Double) -> (Int, Integer)
    findn' (n, k, d)
      | d < 10^(k-1) = findn' (n-1, k, d*10)
      | d >= 10^k    = findn' (n+1, k, d/10)
      | otherwise = (n, round d)

