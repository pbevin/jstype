module Builtins.Number (makeNumberClass) where

import Control.Lens
import Safe
import Text.Printf
import Runtime
import Data.Maybe

makeNumberClass :: Runtime (Shared JSObj)
makeNumberClass = do
  numberPrototype <- makePrototype "Number"
    >>= addMethod "toFixed"     1 toFixed
    >>= addMethod "constructor" 1 numberConstructor
    >>= addMethod "toString"    0 numberToString
    >>= addMethod "valueOf"     0 numberValueOf

  functionObject "Number" numberPrototype
    >>= setCallMethod numberFunction
    >>= setCstrMethod numberConstructor
    >>= addMethod "isNaN" 1 objIsNaN
    >>= addReadOnlyConstants numberConstants

numberConstants :: [(String, JSNum)]
numberConstants = [ ("NaN", jsNaN),
                    ("POSITIVE_INFINITY",  jsInf),
                    ("NEGATIVE_INFINITY", -jsInf),
                    ("MAX_VALUE", jsMaxValue),
                    ("MIN_VALUE", jsMinValue) ]

-- ref 15.7.4.5, incomplete
toFixed :: JSFunction
toFixed this args = do
  fractionDigits <- toInt (first1 args)
  let fmt = "%." ++ show fractionDigits ++ "f"
  x <- toNumber this
  return $ VStr $ printf fmt x

numberToString :: JSFunction
numberToString this _args =
  case this of
    VNum num -> return . VStr . showVal $ this
    VInt num -> return . VStr . showVal $ this
    VObj obj -> do
      cls <- view objClass <$> deref obj
      if cls == "Number"
      then VStr . maybe "" showVal . view objPrimitiveValue <$> deref obj
      else error
    _ -> error
  where error = raiseTypeError "Not a number"

numberValueOf :: JSFunction
numberValueOf this _args =
  case this of
    VNum num -> return this
    VInt num -> return this
    VObj obj -> do
      cls <- view objClass <$> deref obj
      if cls == "Number"
      then fromMaybe (VInt 0) . view objPrimitiveValue <$> deref obj
      else error
    _ -> error
  where error = raiseTypeError "Not a number"


numberFunction :: JSFunction
numberFunction this args =
  let val = headDef (VInt 0) args
  in toNum val


numberConstructor :: JSFunction
numberConstructor this args = 
  let val = headDef (VInt 0) args
  in case this of
    VObj obj -> do
      prototype <- objFindPrototype "Number"
      num <- VNum <$> toNumber val
      setClass "Number" obj
        >>= objSetPrimitive num
        >>= objSetPrototype prototype
      return (VObj obj)
    _ -> raiseError $ "Number constructor called with this = " ++ show this
