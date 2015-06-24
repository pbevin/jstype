module Builtins.Boolean (makeBooleanClass) where

import Control.Lens
import Safe
import Runtime
import Data.Maybe


makeBooleanClass :: Runtime (Shared JSObj)
makeBooleanClass = do
  booleanPrototype <- makePrototype "Boolean"
    >>= addMethod "constructor" 1 booleanConstructor
    >>= addMethod "toString"    1 booleanToString
    >>= addMethod "valueOf"     1 booleanValueOf

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
        >>= objSetPrimitive prim
        >>= objSetPrototype prototype
      return (VObj obj)
    _ -> raiseError $ "Boolean constructor called with this = " ++ show this


booleanToString :: JSFunction
booleanToString this args = VStr . showVal <$> booleanValueOf this args

booleanValueOf :: JSFunction
booleanValueOf this _args = do
  assertType TypeBoolean this
  b <- case this of
    VBool _ -> return this
    VObj obj ->
      fromMaybe (VBool False) . view objPrimitiveValue <$> deref obj
  return b
