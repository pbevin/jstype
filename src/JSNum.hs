module JSNum where

newtype JSNum = JSNum Double deriving (Show, Read)
instance Eq JSNum where
  JSNum a == JSNum b = a == b || abs (a-b) < 0.001

fromJSNum :: JSNum -> Double
fromJSNum (JSNum a) = a

jsNaN :: JSNum
jsNaN = JSNum $ 0 / 0

jsMaxValue :: JSNum
jsMaxValue = JSNum $ maxNonInfiniteFloat 1.0

jsMinValue :: JSNum
jsMinValue = JSNum $ minPositiveFloat 1.0


instance Num JSNum where
  (JSNum a) + (JSNum b) = JSNum (a + b)
  (JSNum a) - (JSNum b) = JSNum (a - b)
  (JSNum a) * (JSNum b) = JSNum (a * b)
  fromInteger n = JSNum $ fromInteger n
  abs (JSNum a) = JSNum $ abs a
  signum (JSNum a) = JSNum $ signum a
  negate (JSNum a) = JSNum (negate a)

instance Fractional JSNum where
  (JSNum a) / (JSNum b) = JSNum (a / b)
  fromRational r = JSNum $ fromRational r

instance Ord JSNum where
  compare (JSNum a) (JSNum b) = compare a b

instance Real JSNum where
  toRational (JSNum a) = toRational a

instance RealFrac JSNum where
  properFraction (JSNum a) = let (x, y) = properFraction a in (x, JSNum y)


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
