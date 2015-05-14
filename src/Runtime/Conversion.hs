module Runtime.Conversion where

import Runtime.Types

-- ref 9.2, incomplete
toBoolean :: JSVal -> Bool
toBoolean VUndef    = False
toBoolean VNull     = False
toBoolean (VBool b) = b
toBoolean (VNum n)  = n /= 0  -- s/b +0, -0 or NaN
toBoolean (VStr "") = False
toBoolean _         = True
