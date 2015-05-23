module JSNum where

newtype JSNum = JSNum Double deriving (Show, Read)
instance Eq JSNum where
  JSNum a == JSNum b = a == b || abs (a-b) < 0.001

fromJSNum :: JSNum -> Double
fromJSNum (JSNum a) = a

jsNaN :: JSNum
jsNaN = JSNum $ 0 / 0


instance Num JSNum where
  (JSNum a) + (JSNum b) = JSNum (a + b)
  (JSNum a) - (JSNum b) = JSNum (a - b)
  (JSNum a) * (JSNum b) = JSNum (a * b)
  fromInteger n = JSNum $ fromInteger n
  abs (JSNum a) = JSNum $ abs a
  signum (JSNum a) = JSNum $ signum a

instance Fractional JSNum where
  (JSNum a) / (JSNum b) = JSNum (a / b)
  fromRational r = JSNum $ fromRational r

instance Ord JSNum where
  compare (JSNum a) (JSNum b) = compare a b

instance Real JSNum where
  toRational (JSNum a) = toRational a

instance RealFrac JSNum where
  properFraction (JSNum a) = let (x, y) = properFraction a in (x, JSNum y)
