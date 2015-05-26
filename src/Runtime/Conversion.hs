module Runtime.Conversion where

import Data.Maybe
import Safe
import JSNum
import Runtime.Types
import Runtime.Object


-- ref 9.1, incomplete
toPrimitive :: PrimitiveHint -> JSVal -> Runtime JSVal
toPrimitive hint a = case a of
  VObj obj -> objDefaultValue hint =<< deref obj
  _        -> return a

-- ref 9.2, incomplete
toBoolean :: JSVal -> Bool
toBoolean VUndef    = False
toBoolean VNull     = False
toBoolean (VBool b) = b
toBoolean (VNum n)  = n /= 0 && not (isNaN $ fromJSNum n)
toBoolean (VStr "") = False
toBoolean _         = True

-- ref 9.3, incomplete
toNumber :: JSVal -> Runtime JSNum
toNumber VUndef     = return jsNaN
toNumber VNull      = return 0
toNumber (VBool b)  = return $ if b then 1 else 0
toNumber (VNum n)   = return n
toNumber (VStr s)   = return $ JSNum (fromMaybe 0 $ readMay s)
toNumber v@(VObj _) = toNumber =<< toPrimitive HintNumber v
toNumber _ = return 0

isString :: JSVal -> Bool
isString (VStr _) = True
isString _ = False

showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) = numberToString n
showVal (VBool a) = if a then "true" else "false"
showVal (VRef ref) = "(reference " ++ show ref ++ ")"
showVal VUndef = "undefined"
showVal VNull  = "null"
showVal (VObj _) = "[Object object]"
showVal (VNative _) = "(native function)"
showVal (VStacktrace st) = "Stacktrace " ++ show st

toString :: JSVal -> Runtime String
toString VUndef    = return "undefined"
toString VNull     = return "null"
toString (VBool b) = return $ if b then "true" else "false"
toString (VStr s)  = return s
toString (VNum n)  = return $ numberToString $ fromJSNum n
toString (VStacktrace st) = return $ unlines (map show st)
toString other = return $ show other

numberToString :: Double -> String
numberToString n
  | n == 0/0  = "NaN"
  | n == 1/0  = "Infinity"
  | n < 0     = "-" ++ numberToString (negate n)
  | isInteger n = show (round n :: Integer)
  | otherwise = show n

isInteger :: RealFloat a => a -> Bool
isInteger n = n == fromInteger (round n)
