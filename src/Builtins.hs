module Builtins where

import Control.Monad
import Control.Arrow
import Text.Printf
import Safe
import Runtime
import Builtins.Date
import Builtins.Number
import Builtins.Boolean
import Builtins.String
import Builtins.Array

configureBuiltins :: Shared JSObj -> Runtime ()
configureBuiltins obj = do
  prototype <- getGlobalObjectPrototype

  string <- makeStringClass
  boolean <- makeBooleanClass
  number <- makeNumberClass
  array <- makeArrayClass

  errorPrototype <- newObject
    >>= setClass "Error"
    >>= addOwnProperty "toString" (VNative errorToString)
    >>= addOwnProperty "prototype" (VObj prototype)

  errorObj <- functionObject "Error" errorPrototype
    >>= setCallMethod (errFunction errorPrototype)
    >>= setCstrMethod errConstructor


  evalError      <- errorSubtype "EvalError" (VObj errorPrototype)
  rangeError     <- errorSubtype "RangeError" (VObj errorPrototype)
  referenceError <- errorSubtype "ReferenceError" (VObj errorPrototype)
  syntaxError    <- errorSubtype "SyntaxError" (VObj errorPrototype)
  typeError      <- errorSubtype "TypeError" (VObj errorPrototype)
  uriError       <- errorSubtype "URIError" (VObj errorPrototype)

  date <- makeDateClass

  math <- mathObject
  json <- newObject
    >>= addOwnProperty "stringify" (VNative jsonStringify)

  regexpPrototype <- newObject
    >>= setClass "RegExp"
    >>= objSetPrototype prototype
    >>= addOwnProperty "exec" (VNative regexpExec)

  regexp <- newObject
    >>= setClass "Function"
    >>= setCallMethod (regexpFunction regexpPrototype)
    >>= setCstrMethod regexpConstructor
    >>= addOwnProperty "prototype" (VObj regexpPrototype)

  console <- newObject
    >>= addOwnProperty "log" (VNative jsConsoleLog)

  addOwnProperty "escape" (VNative objEscape) obj
    >>= addOwnProperty "console" (VObj console)
    >>= addOwnProperty "String" (VObj string)
    >>= addOwnProperty "Number" (VObj number)
    >>= addOwnProperty "Boolean" (VObj boolean)
    >>= addOwnProperty "Array" (VObj array)
    >>= addOwnProperty "Date" (VObj date)
    >>= addOwnProperty "RegExp" (VObj regexp)
    >>= addOwnProperty "Error" (VObj errorObj)
    >>= addOwnProperty "EvalError" (VObj evalError)
    >>= addOwnProperty "RangeError" (VObj rangeError)
    >>= addOwnProperty "ReferenceError" (VObj referenceError)
    >>= addOwnProperty "SyntaxError" (VObj syntaxError)
    >>= addOwnProperty "TypeError" (VObj typeError)
    >>= addOwnProperty "URIError" (VObj uriError)
    >>= addOwnProperty "Math" (VObj math)
    >>= addOwnProperty "JSON" (VObj json)
    >>= addOwnProperty "eval" (VNative objEval)
    >>= addOwnProperty "isNaN" (VNative objIsNaN)
    >>= addOwnProperty "isFinite" (VNative objIsFinite)
    >>= addOwnProperty "parseInt" (VNative parseInt)
    >>= addOwnProperty "parseFloat" (VNative parseFloat)
    >>= addOwnConstant "Infinity" (VNum $ 1 / 0)
    >>= addOwnConstant "NaN" (VNum $ jsNaN)
    >>= addOwnConstant "undefined" (VUndef)
    >>= addOwnConstant "null" (VNull)

  return ()


mathObject :: Runtime (Shared JSObj)
mathObject = do
  newObject >>= addReadOnlyConstants mathConstants
            >>= addOwnProperty "abs" (VNative $ mathFunc abs)
            >>= addOwnProperty "log" (VNative $ mathFunc log)
            >>= addOwnProperty "exp" (VNative $ mathFunc exp)
            >>= addOwnProperty "sin" (VNative $ mathFunc sin)
            >>= addOwnProperty "cos" (VNative $ mathFunc cos)
            >>= addOwnProperty "tan" (VNative $ mathFunc tan)
            >>= addOwnProperty "asin" (VNative $ mathFunc asin)
            >>= addOwnProperty "acos" (VNative $ mathFunc acos)
            >>= addOwnProperty "atan" (VNative $ mathFunc atan)
            >>= addOwnProperty "sqrt" (VNative $ mathFunc sqrt)
            >>= addOwnProperty "ceil" (VNative $ mathFunc $ fromIntegral . ceiling)
            >>= addOwnProperty "round" (VNative $ mathFunc $ fromIntegral . round)
            >>= addOwnProperty "floor" (VNative $ mathFunc $ fromIntegral . floor)
            >>= addOwnProperty "trunc" (VNative $ mathFunc $ fromIntegral . truncate)
            >>= addOwnProperty "random" (VNative $ mathFunc $ const 4) -- xkcd #221

            >>= addOwnProperty "max" (VNative $ mathMaxFunc maximum)
            >>= addOwnProperty "min" (VNative $ mathMaxFunc minimum)

            >>= addOwnProperty "pow" (VNative $ mathFunc2 pow)
            >>= addOwnProperty "atan2" (VNative $ mathFunc2 atan2)
            >>= addOwnProperty "hypot" (VNative $ mathFunc2 hypot)

mathConstants :: [(String, JSNum)]
mathConstants = allToJSNum [ ("PI", pi),
                             ("SQRT2", sqrt 2),
                             ("SQRT1_2", sqrt 0.5),
                             ("E", exp 1),
                             ("LN2", log 2),
                             ("LN10", log 10),
                             ("LOG10E", 1 / log 10),
                             ("LOG2E", 1 / log 2) ]
  where allToJSNum = map (second JSNum)


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

functionIsConstructor :: JSFunction -> Shared JSObj -> JSFunction
functionIsConstructor cstr proto _this args = do
  this <- newObject >>= objSetPrototype proto
  cstr (VObj this) args

regexpFunction :: Shared JSObj -> JSVal -> [JSVal] -> Runtime JSVal
regexpFunction = functionIsConstructor regexpConstructor

regexpConstructor :: JSVal -> [JSVal] -> Runtime JSVal
regexpConstructor this _ = case this of
  VObj objRef -> VObj <$> (setClass "RegExp" objRef)

-- ref 15.10.6.2, incomplete
regexpExec :: JSVal -> [JSVal] -> Runtime JSVal
regexpExec _ _ = return VNull
