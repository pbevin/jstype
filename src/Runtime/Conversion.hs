module Runtime.Conversion where

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
toNumber :: JSVal -> JSNum
toNumber VUndef     = jsNaN -- s/b NaN
toNumber VNull      = 0
toNumber (VBool b)  = if b then 1 else 0
toNumber (VNum n)   = n
toNumber (VStr s)   = JSNum $ read s
toNumber (VObj obj) = error "Can't toNumber on an object yet" -- toPrimitive HintNumber obj
toNumber _ = 0

isString :: JSVal -> Bool
isString (VStr _) = True
isString _ = False

toString :: JSVal -> String
toString (VStr s) = s
toString (VNum n) = show n
toString _ = "???"
