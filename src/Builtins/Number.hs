{-# LANGUAGE OverloadedStrings #-}

module Builtins.Number (makeNumberClass) where

import Control.Lens
import Safe
import Text.Printf
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Runtime
import Polyvariadic
import JSNum

makeNumberClass :: Runtime (Shared JSObj)
makeNumberClass = do
  numberPrototype <- mkObject $ do
    className "Number"
    method "toFixed"     1 toFixed
    method "toString"    0 numberToString
    method "valueOf"     0 numberValueOf

  functionPrototype <- objFindPrototype "Function"
  number <- mkObject $ do
    className "Function"
    prototype functionPrototype
    property "prototype" (VObj numberPrototype)
    property "length" (VInt 1)
    internal callMethod numberFunction
    internal cstrMethod numberConstructor
    internal hasInstanceMethod funHasInstance
    method "isNaN" 1 objIsNaN
    constant "NaN"        jsNaN
    constant "POSITIVE_INFINITY" jsInf
    constant "NEGATIVE_INFINITY" (-jsInf)
    constant "MAX_VALUE" jsMaxValue
    constant "MIN_VALUE" jsMinValue

  addOwnProperty "constructor" (VObj number) numberPrototype

  return number
numberConstants :: [(Text, JSNum)]
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
  return . VStr . T.pack $ printf fmt x

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
    _ -> raiseError . T.pack $ "Number constructor called with this = " ++ show this
