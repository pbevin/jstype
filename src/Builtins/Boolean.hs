module Builtins.Boolean (makeBooleanClass) where

import Safe
import Runtime


makeBooleanClass :: Runtime (Shared JSObj)
makeBooleanClass = do
  booleanPrototype <- makePrototype "Boolean"
    >>= addOwnProperty "constructor" (VNative 1 booleanConstructor)

  functionObject "Boolean" booleanPrototype
    >>= setCallMethod booleanFunction
    >>= setCstrMethod booleanConstructor
    >>= addOwnProperty "constructor" (VNative 1 booleanConstructor)


booleanFunction :: JSFunction
booleanFunction this args =
  let val = headDef (VNum 0) args
  in return val


booleanConstructor :: JSFunction
booleanConstructor this args =
  let val = headDef (VNum 0) args
  in case this of
    VObj obj -> do
      prototype <- objFindPrototype "Boolean"
      str <- toString val
      setClass "Boolean" obj
        >>= setPrimitiveValue val
        >>= setPrimitiveToString (VStr str)
        >>= objSetPrototype prototype
      return (VObj obj)
    _ -> raiseError $ "Boolean constructor called with this = " ++ show this
