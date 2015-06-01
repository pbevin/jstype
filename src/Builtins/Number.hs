module Builtins.Number (makeNumberClass) where

import Safe
import Text.Printf
import Runtime

makeNumberClass :: Runtime (Shared JSObj)
makeNumberClass = do
  numberPrototype <- makePrototype "Number"
    >>= addOwnProperty "toFixed" (VNative toFixed)
    >>= addOwnProperty "constructor" (VNative numberConstructor)

  functionObject "Number" numberPrototype
    >>= setCallMethod numberFunction
    >>= setCstrMethod numberConstructor
    >>= addOwnProperty "isNaN" (VNative objIsNaN)
    >>= addReadOnlyConstants numberConstants

numberConstants :: [(String, JSNum)]
numberConstants = [ ("NaN", JSNum $ 0/0),
                    ("POSITIVE_INFINITY", JSNum $ 1/0),
                    ("NEGATIVE_INFINITY", JSNum $ -1/0),
                    ("MAX_VALUE", jsMaxValue),
                    ("MIN_VALUE", jsMinValue) ]

-- ref 15.7.4.5, incomplete
toFixed :: JSFunction
toFixed this args = do
  fractionDigits <- toInt (first1 args)
  let fmt = "%." ++ show fractionDigits ++ "f"
  x <- toNumber this
  return $ VStr $ printf fmt (fromJSNum x)


numberFunction :: JSFunction
numberFunction this args =
  let val = headDef (VNum 0) args
  in return val


numberConstructor :: JSFunction
numberConstructor this args = 
  let val = headDef (VNum 0) args
  in case this of
    VObj obj -> do
      prototype <- objFindPrototype "Number"
      str <- toString val
      setClass "Number" obj
        >>= setPrimitiveValue val
        >>= setPrimitiveToString (VStr str)
        >>= objSetPrototype prototype
      return (VObj obj)
    _ -> raiseError $ "Number constructor called with this = " ++ show this
