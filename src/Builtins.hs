module Builtins where

import Control.Monad
import Control.Arrow
import Text.Printf
import Safe
import Runtime
import Builtins.Array
import Builtins.String
import Builtins.Boolean
import Builtins.Number
import Builtins.Math
import Builtins.Date
import Builtins.RegExp

configureBuiltins :: Shared JSObj -> Runtime ()
configureBuiltins obj = do
  prototype <- getGlobalObjectPrototype

  array     <- makeArrayClass     -- 15.4
  string    <- makeStringClass    -- 15.5
  boolean   <- makeBooleanClass   -- 15.6
  number    <- makeNumberClass    -- 15.7
  math      <- mathObject         -- 15.8
  date      <- makeDateClass      -- 15.9
  regexp    <- makeRegExpClass    -- 15.10
  json      <- newObject          -- 15.12
    >>= addMethod      "stringify"           1 jsonStringify

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

  addMethod            "escape"              1 objEscape obj
    >>= addMethod      "eval"                1 (objEval IndirectEvalCall)
    >>= addMethod      "isNaN"               1 objIsNaN
    >>= addMethod      "isFinite"            1 objIsFinite
    >>= addMethod      "parseInt"            1 parseInt
    >>= addMethod      "parseFloat"          1 parseFloat
    >>= addOwnProperty "console"        ( VObj console          )
    >>= addOwnProperty "String"         ( VObj string           )
    >>= addOwnProperty "Number"         ( VObj number           )
    >>= addOwnProperty "Boolean"        ( VObj boolean          )
    >>= addOwnProperty "Array"          ( VObj array            )
    >>= addOwnProperty "Date"           ( VObj date             )
    >>= addOwnProperty "RegExp"         ( VObj regexp           )
    >>= addOwnProperty "Error"          ( VObj errorObj         )
    >>= addOwnProperty "EvalError"      ( VObj evalError        )
    >>= addOwnProperty "RangeError"     ( VObj rangeError       )
    >>= addOwnProperty "ReferenceError" ( VObj referenceError   )
    >>= addOwnProperty "SyntaxError"    ( VObj syntaxError      )
    >>= addOwnProperty "TypeError"      ( VObj typeError        )
    >>= addOwnProperty "URIError"       ( VObj uriError         )
    >>= addOwnProperty "Math"           ( VObj math             )
    >>= addOwnProperty "JSON"           ( VObj json             )
    >>= addOwnConstant "Infinity"       ( VNum $ 1 / 0          )
    >>= addOwnConstant "NaN"            ( VNum $ jsNaN          )
    >>= addOwnConstant "undefined"      ( VUndef                )
    >>= addOwnConstant "null"           ( VNull                 )

  return ()


errorSubtype :: String -> JSVal -> Runtime (Shared JSObj)
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
