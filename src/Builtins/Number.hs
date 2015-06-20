module Builtins.Number (makeNumberClass) where

import Safe
import Text.Printf
import Runtime

makeNumberClass :: Runtime (Shared JSObj)
makeNumberClass = do
  numberPrototype <- makePrototype "Number"
    >>= addMethod "Number.toFixed"     1 toFixed
    >>= addMethod "Number.constructor" 1 numberConstructor
    >>= addMethod "Number.toString"    0 numberToString

  functionObject "Number" numberPrototype
    >>= setCallMethod numberFunction
    >>= setCstrMethod numberConstructor
    >>= addMethod "isNaN" 1 objIsNaN
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

numberToString :: JSFunction
numberToString this _args =
  case this of
    VNum num -> return . VStr $ show num
    VObj obj -> do
      cls <- objClass <$> deref obj
      if cls == "Number"
      then VStr . maybe "" showVal . objPrimitiveValue <$> deref obj
      else error
    _ -> error
  where error = raiseTypeError "Not a number"


numberFunction :: JSFunction
numberFunction this args =
  let val = headDef (VNum 0) args
  in VNum <$> toNumber val


numberConstructor :: JSFunction
numberConstructor this args = 
  let val = headDef (VNum 0) args
  in case this of
    VObj obj -> do
      prototype <- objFindPrototype "Number"
      num <- VNum <$> toNumber val
      str <- VStr <$> toString num
      setClass "Number" obj
        >>= setPrimitiveValue num
        >>= objSetPrototype prototype
      return (VObj obj)
    _ -> raiseError $ "Number constructor called with this = " ++ show this
