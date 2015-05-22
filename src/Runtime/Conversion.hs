module Runtime.Conversion where

import Data.Maybe
import Safe
import Expr
import Runtime.Types
import Runtime.Object


-- ref 9.1, incomplete
toPrimitive :: PrimitiveHint -> JSVal -> JSRuntime JSVal
toPrimitive hint a = case a of
  VObj obj -> objDefaultValue hint =<< deref obj
  _        -> return a

-- ref 9.2, incomplete
toBoolean :: JSVal -> Bool
toBoolean VUndef    = False
toBoolean VNull     = False
toBoolean (VBool b) = b
toBoolean (VNum n)  = n /= 0  -- s/b +0, -0 or NaN
toBoolean (VStr "") = False
toBoolean _         = True

-- ref 9.3, incomplete
toNumber :: JSVal -> JSRuntime JSNum
toNumber VUndef     = return jsNaN
toNumber VNull      = return 0
toNumber (VBool b)  = return $ if b then 1 else 0
toNumber (VNum n)   = return n
toNumber (VStr s)   = return $ JSNum (fromMaybe 0 $ readMay s)
toNumber v@(VObj obj) = toNumber =<< toPrimitive HintNumber v
toNumber _ = return 0

isString :: JSVal -> Bool
isString (VStr _) = True
isString _ = False

showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) =
  if n == fromInteger (round n)
  then show (round n :: Integer)
  else show n
showVal other = show other

toString :: JSVal -> JSRuntime String
toString VUndef    = return "undefined"
toString VNull     = return "null"
toString (VBool b) = return $ if b then "true" else "false"
toString (VStr s)  = return s
toString (VNum n)  = return $ showVal (VNum n)
toString (VRef _) = return "(??ref)"
toString (VObj _) = return "(??obj)"
toString (VNative _) = return "(??native)"
toString (VException _) = return "(??exception)"
