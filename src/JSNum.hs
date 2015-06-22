module JSNum where

type JSNum = Double

jsMaxValue, jsMinValue :: JSNum
jsMaxValue = maxNonInfiniteFloat 1.0
jsMinValue = minPositiveFloat 1.0

jsNaN, jsInf :: JSNum
jsNaN = 0/0
jsInf = 1/0

-- http://stackoverflow.com/a/1780724/183140
maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

minPositiveFloat :: RealFloat a => a -> a
minPositiveFloat a = encodeFloat 1 $ fst (floatRange a) - floatDigits a
