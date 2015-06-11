module Builtins.Boolean (makeBooleanClass) where

import Safe
import Runtime


makeBooleanClass :: Runtime (Shared JSObj)
makeBooleanClass = do
  booleanPrototype <- makePrototype "Boolean"
    >>= addMethod "constructor" 1 booleanConstructor

  functionPrototype <- findPrototypeForClass "Function"

  functionObject "Boolean" booleanPrototype
    >>= setCallMethod booleanFunction
    >>= setCstrMethod booleanConstructor
    >>= objSetPrototype functionPrototype
    >>= addMethod "constructor" 1 booleanConstructor
    >>= addOwnProperty "length" (VNum 1)


booleanFunction :: JSFunction
booleanFunction this args =
  let val = headDef (VBool False) args
  in return $ VBool $ toBoolean val


booleanConstructor :: JSFunction
booleanConstructor this args =
  let val = headDef (VBool False) args
  in case this of
    VObj obj -> do
      prototype <- objFindPrototype "Boolean"
      let prim = VBool (toBoolean val)
      setClass "Boolean" obj
        >>= setPrimitiveValue prim
        >>= objSetPrototype prototype
      return (VObj obj)
    _ -> raiseError $ "Boolean constructor called with this = " ++ show this
