{-# LANGUAGE OverloadedStrings #-}

module Builtins where

import Control.Monad
import Control.Arrow
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf
import Safe
import Runtime
import Polyvariadic
import Builtins.Array
import Builtins.String
import Builtins.Boolean
import Builtins.Number
import Builtins.Math
import Builtins.Date
import Builtins.RegExp
import Builtins.ParseInt
import Builtins.ParseFloat
import Builtins.URI
import JSNum

configureBuiltins :: Runtime ()
configureBuiltins = do
  obj       <- getGlobalObject
  prototype <- getGlobalObjectPrototype

  array     <- makeArrayClass     -- 15.4
  string    <- makeStringClass    -- 15.5
  boolean   <- makeBooleanClass   -- 15.6
  number    <- makeNumberClass    -- 15.7
  math      <- mathObject         -- 15.8
  date      <- makeDateClass      -- 15.9
  regexp    <- makeRegExpClass    -- 15.10
  json      <- mkObject $ do      -- 15.12
    className "JSON"
    method "stringify" 1 jsonStringify

  errorPrototype <- newObject
    >>= setClass       "Error"
    >>= addMethod      "toString"             0 errorToString
    >>= addOwnProperty "prototype"        (VObj prototype)

  errorObj <- functionObject "Error" errorPrototype
    >>= setCallMethod (errFunction errorPrototype)
    >>= setCstrMethod errConstructor


  evalError      <- errorSubtype "EvalError"      ( VObj errorPrototype )
  rangeError     <- errorSubtype "RangeError"     ( VObj errorPrototype )
  referenceError <- errorSubtype "ReferenceError" ( VObj errorPrototype )
  syntaxError    <- errorSubtype "SyntaxError"    ( VObj errorPrototype )
  typeError      <- errorSubtype "TypeError"      ( VObj errorPrototype )
  uriError       <- errorSubtype "URIError"       ( VObj errorPrototype )

  console <- newObject
    >>= addMethod      "log"                 1 jsConsoleLog

  overObject obj $ do

    method   "escape"              1 objEscape
    method   "eval"                1 (objEval IndirectEvalCall)
    method   "isNaN"               1 objIsNaN
    method   "isFinite"            1 objIsFinite
    static   "parseInt"            2 parseInt
    static   "parseFloat"          1 parseFloat
    static   "decodeURI"           1 decodeURI
    static   "decodeURIComponent"  1 decodeURIComponent
    static   "encodeURI"           1 encodeURI
    static   "encodeURIComponent"  1 encodeURIComponent

    property "console"        ( VObj console        )
    property "String"         ( VObj string         )
    property "Number"         ( VObj number         )
    property "Boolean"        ( VObj boolean        )
    property "Array"          ( VObj array          )
    property "Date"           ( VObj date           )
    property "RegExp"         ( VObj regexp         )
    property "Error"          ( VObj errorObj       )
    property "EvalError"      ( VObj evalError      )
    property "RangeError"     ( VObj rangeError     )
    property "ReferenceError" ( VObj referenceError )
    property "SyntaxError"    ( VObj syntaxError    )
    property "TypeError"      ( VObj typeError      )
    property "URIError"       ( VObj uriError       )
    property "Math"           ( VObj math           )
    property "JSON"           ( VObj json           )
    constant "Infinity"       ( VNum $ 1 / 0        )
    constant "NaN"            ( VNum $ jsNaN        )
    constant "undefined"      ( VUndef              )
    constant "null"           ( VNull               )

  return ()


errorSubtype :: Text -> JSVal -> Runtime (Shared JSObj)
errorSubtype name parentPrototype = do
  prototype <-
    newObject >>= setClass "Error"
              >>= objSetPrototype (toObj parentPrototype)
              >>= addOwnProperty "prototype" parentPrototype
              >>= addOwnProperty "name" (VStr name)

  functionObject name prototype
            >>= setCallMethod (errFunction prototype)
            >>= setCstrMethod errConstructor


jsonStringify :: JSVal -> [JSVal] -> Runtime JSVal
jsonStringify _this _args = return $ VStr "not implemented"

objIsFinite :: JSFunction
objIsFinite _this args =
  let arg = first1 args
  in VBool . isFinite <$> toNumber arg
    where isFinite x
            | isNaN x   = False
            | x == 1/0  = False
            | x == -1/0 = False
            | otherwise = True
