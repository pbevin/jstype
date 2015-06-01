module Builtins where

import Control.Monad
import Control.Arrow
import Text.Printf
import Safe
import Builtins.Date
import Runtime
import JSNum

configureBuiltins :: Shared JSObj -> Runtime ()
configureBuiltins obj = do
  prototype <- getGlobalObjectPrototype

  stringPrototype <- newObject
    >>= setClass "String"
    >>= addOwnProperty "charAt" (VNative stringCharAt)
    >>= addOwnProperty "toString" (VNative stringToString)

  string <- functionObject "String" stringPrototype
    >>= setCallMethod strFunction
    >>= setCstrMethod (strConstructor stringPrototype)

  booleanPrototype <- newObject
    >>= setClass "Boolean"

  boolean <- functionObject "Boolean" booleanPrototype
    >>= isWrapperFor (return . VBool . toBoolean) (VBool False) booleanPrototype "Boolean"

  numberPrototype <- newObject
    >>= setClass "Number"
    >>= addOwnProperty "toFixed" (VNative toFixed)

  number <- functionObject "Number" numberPrototype
    >>= isWrapperFor (\s -> VNum <$> toNumber s) (VNum 0) numberPrototype "Number"
    >>= addOwnProperty "isNaN" (VNative objIsNaN)
    >>= addReadOnlyConstants numberConstants

  arrayPrototype <- newObject
    >>= setClass "Array"
    >>= objSetPrototype prototype
    >>= addOwnProperty "prototype" (VObj prototype)
    >>= addOwnProperty "length" (VNum 0)
    >>= addOwnProperty "toString" (VNative arrayToString)
    >>= addOwnProperty "reduce" (VNative arrayReduce)

  array <- functionObject "Array" arrayPrototype
    >>= setCallMethod (arrayFunction arrayPrototype)
    >>= setCstrMethod arrayConstructor

  errorPrototype <- newObject
    >>= setClass "Error"
    >>= addOwnProperty "toString" (VNative errorToString)
    >>= addOwnProperty "prototype" (VObj prototype)

  errorObj <- functionObject "Error" errorPrototype
    >>= setCallMethod (errFunction errorPrototype)
    >>= setCstrMethod errConstructor


  referenceError <- errorSubtype "ReferenceError" (VObj errorPrototype)
  syntaxError    <- errorSubtype "SyntaxError" (VObj errorPrototype)
  typeError      <- errorSubtype "TypeError" (VObj errorPrototype)

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
    >>= addOwnProperty "Error" (VObj errorObj)
    >>= addOwnProperty "Date" (VObj date)
    >>= addOwnProperty "RegExp" (VObj regexp)
    >>= addOwnProperty "ReferenceError" (VObj referenceError)
    >>= addOwnProperty "SyntaxError" (VObj syntaxError)
    >>= addOwnProperty "TypeError" (VObj typeError)
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


numberConstants :: [(String, JSNum)]
numberConstants = [ ("NaN", JSNum $ 0/0),
                    ("POSITIVE_INFINITY", JSNum $ 1/0),
                    ("NEGATIVE_INFINITY", JSNum $ -1/0),
                    ("MAX_VALUE", jsMaxValue),
                    ("MIN_VALUE", jsMinValue) ]

addReadOnlyConstants :: [(String, JSNum)] -> Shared JSObj -> Runtime (Shared JSObj)
addReadOnlyConstants xs obj = do
  forM xs $ \(name, value) -> addOwnConstant name (VNum value) obj
  return obj

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


-- ref 15.7.4.5, incomplete
toFixed :: JSFunction
toFixed this args = do
  fractionDigits <- toInt (first1 args)
  let fmt = "%." ++ show fractionDigits ++ "f"
  x <- toNumber this
  return $ VStr $ printf fmt (fromJSNum x)

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

-- ref 15.4.4.2, incomplete
arrayToString :: JSVal -> [JSVal] -> Runtime JSVal
arrayToString _this _args = return $ VStr "[...]"

-- ref 15.4.4.21, incomplete
arrayReduce :: JSVal -> [JSVal] -> Runtime JSVal
arrayReduce _this _args = return VNull

-- ref 15.5.1.1
strFunction :: JSFunction
strFunction this args =
  let value = firstArg (VStr "") args
  in VStr <$> toString value

-- ref 15.5.2.1
strConstructor :: Shared JSObj -> JSFunction
strConstructor prototype this args =
  let value = firstArg  (VStr "") args
  in case this of
    VObj obj -> do
      str <- toString value
      setClass "String" obj >>= objSetPrototype prototype
                            >>= objSetExtensible True
                            >>= objSetPrimitive value
                            >>= addOwnProperty "length" (VNum $ fromIntegral $ length str)
      return this

-- ref 15.5.4.2
stringToString :: JSFunction
stringToString this _args = do
  case this of
    VObj obj -> objGetPrimitive obj

-- ref 15.5.4.4, incomplete
stringCharAt :: JSFunction
stringCharAt this args =
  let pos = first1 args
  in do
    checkObjectCoercible this
    str <- toString this
    position <- toInt pos
    return $ maybe VUndef charToStr $ atMay str position
      where charToStr ch = VStr [ch]



isWrapperFor :: (JSVal -> Runtime JSVal) -> JSVal -> Shared JSObj -> String -> ObjectModifier
isWrapperFor f defaultValue prototype name obj =
  setCallMethod call obj >>= setCstrMethod cstr
  where
    call _this args =
      if null args then return defaultValue else f (head args)
    cstr this args = do
      val <- call this args
      case this of
        VObj obj -> do
          str <- toString val
          setClass name obj >>= setPrimitiveValue val
                            >>= setPrimitiveToString (VStr str)
                            >>= objSetPrototype prototype
          return (VObj obj)
        _ -> raiseError $ name ++ " constructor called with this = " ++ show this
