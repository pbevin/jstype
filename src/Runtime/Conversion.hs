module Runtime.Conversion where

import Expr
import Runtime.Types

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
toNumber VUndef     = 0 -- s/b NaN
toNumber VNull      = 0
toNumber (VBool b)  = if b then 1 else 0
toNumber (VNum n)   = n
toNumber (VStr s)   = JSNum $ read s
toNumber (VObj obj) = 0 -- XXX
