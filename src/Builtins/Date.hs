module Builtins.Date (makeDateClass) where

import Runtime.Types
import Runtime.Object
import Runtime.Error
import Runtime.Conversion
import Runtime.Function
import Runtime.Prototype

makeDatePrototype :: Runtime (Shared JSObj)
makeDatePrototype =
  makePrototype "Date"
    >>= setCstrMethod dateConstructor
    >>= addOwnProperty "constructor" (VNative dateConstructor)
    >>= addOwnProperty "toString" (VNative dateToString)
    >>= addOwnProperty "valueOf" (VNative dateValueOf)
    >>= addOwnProperty "getTime" (VNative dateGetTime)
    >>= addOwnProperty "getFullYear" (VNative dateGetFullYear)
    >>= addOwnProperty "getUTCFullYear" (VNative dateGetUTCFullYear)
    >>= addOwnProperty "getMonth" (VNative dateGetMonth)
    >>= addOwnProperty "getUTCMonth" (VNative dateGetUTCMonth)
    >>= addOwnProperty "getDate" (VNative dateGetDate)
    >>= addOwnProperty "getUTCDate" (VNative dateGetUTCDate)
    >>= addOwnProperty "getDay" (VNative dateGetDay)
    >>= addOwnProperty "getUTCDay" (VNative dateGetUTCDay)
    >>= addOwnProperty "getHours" (VNative dateGetHours)
    >>= addOwnProperty "getUTCHours" (VNative dateGetUTCHours)
    >>= addOwnProperty "getMinutes" (VNative dateGetMinutes)
    >>= addOwnProperty "getUTCMinutes" (VNative dateGetUTCMinutes)
    >>= addOwnProperty "getSeconds" (VNative dateGetSeconds)
    >>= addOwnProperty "getUTCSeconds" (VNative dateGetUTCSeconds)
    >>= addOwnProperty "getMilliseconds" (VNative dateGetMilliseconds)
    >>= addOwnProperty "getUTCMilliseconds" (VNative dateGetUTCMilliseconds)
    >>= addOwnProperty "setTime" (VNative dateSetTime)
    >>= addOwnProperty "setFullYear" (VNative dateSetFullYear)
    >>= addOwnProperty "setUTCFullYear" (VNative dateSetUTCFullYear)
    >>= addOwnProperty "setMonth" (VNative dateSetMonth)
    >>= addOwnProperty "setUTCMonth" (VNative dateSetUTCMonth)
    >>= addOwnProperty "setDate" (VNative dateSetDate)
    >>= addOwnProperty "setUTCDate" (VNative dateSetUTCDate)
    >>= addOwnProperty "setDay" (VNative dateSetDay)
    >>= addOwnProperty "setUTCDay" (VNative dateSetUTCDay)
    >>= addOwnProperty "setHours" (VNative dateSetHours)
    >>= addOwnProperty "setUTCHours" (VNative dateSetUTCHours)
    >>= addOwnProperty "setMinutes" (VNative dateSetMinutes)
    >>= addOwnProperty "setUTCMinutes" (VNative dateSetUTCMinutes)
    >>= addOwnProperty "setSeconds" (VNative dateSetSeconds)
    >>= addOwnProperty "setUTCSeconds" (VNative dateSetUTCSeconds)
    >>= addOwnProperty "setMilliseconds" (VNative dateSetMilliseconds)
    >>= addOwnProperty "setUTCMilliseconds" (VNative dateSetUTCMilliseconds)
    >>= addOwnProperty "toLocaleString" (VNative dateToLocaleString)
    >>= addOwnProperty "toUTCString" (VNative dateToUTCString)


makeDateClass :: Runtime (Shared JSObj)
makeDateClass = do
  datePrototype <- makeDatePrototype

  functionObject "Date" datePrototype
    >>= setCallMethod dateFunction
    >>= setCstrMethod dateConstructor
    >>= addOwnProperty "prototype" (VObj datePrototype)
    >>= addOwnProperty "parse" (VNative dateParse)
    >>= addOwnProperty "UTC" (VNative dateUTC)

dateFunction :: JSVal -> [JSVal] -> Runtime JSVal
dateFunction _this _args = return $ VStr "[today's date]"

dateConstructor :: JSVal -> [JSVal] -> Runtime JSVal
dateConstructor this _ = case this of
  VObj objRef -> VObj <$> (setClass "Date" objRef)

dateToString :: JSVal -> [JSVal] -> Runtime JSVal
dateToString _ _ = return $ VStr "1969-12-30 21:18:57 UTC"

dateValueOf :: JSVal -> [JSVal] -> Runtime JSVal
dateValueOf _ _ = return $ VNum 142857

dateParse :: JSVal -> [JSVal] -> Runtime JSVal
dateParse _this _args = do
  prototype <- objFindPrototype "Date"
  VObj <$> (newObject >>= objSetPrototype prototype)

dateUTC :: JSVal -> [JSVal] -> Runtime JSVal
dateUTC _this _args = do
  prototype <- objFindPrototype "Date"
  VObj <$> (newObject >>= objSetPrototype prototype)

dateGetTime :: JSFunction
dateGetTime _this _args = return $ VNum 22

dateGetFullYear :: JSFunction
dateGetFullYear _this _args = return $ VNum 2015

dateGetUTCFullYear :: JSFunction
dateGetUTCFullYear _this _args = return $ VNum 2015

dateGetMonth :: JSFunction
dateGetMonth _this _args = return $ VNum 2015

dateGetUTCMonth :: JSFunction
dateGetUTCMonth _this _args = return $ VNum 2015

dateGetDate :: JSFunction
dateGetDate _this _args = return $ VNum 2015

dateGetUTCDate :: JSFunction
dateGetUTCDate _this _args = return $ VNum 2015

dateGetDay :: JSFunction
dateGetDay _this _args = return $ VNum 2015

dateGetUTCDay :: JSFunction
dateGetUTCDay _this _args = return $ VNum 2015

dateGetHours :: JSFunction
dateGetHours _this _args = return $ VNum 2015

dateGetUTCHours :: JSFunction
dateGetUTCHours _this _args = return $ VNum 2015

dateGetMinutes :: JSFunction
dateGetMinutes _this _args = return $ VNum 2015

dateGetUTCMinutes :: JSFunction
dateGetUTCMinutes _this _args = return $ VNum 2015

dateGetSeconds :: JSFunction
dateGetSeconds _this _args = return $ VNum 2015

dateGetUTCSeconds :: JSFunction
dateGetUTCSeconds _this _args = return $ VNum 2015

dateGetMilliseconds :: JSFunction
dateGetMilliseconds _this _args = return $ VNum 2015

dateGetUTCMilliseconds :: JSFunction
dateGetUTCMilliseconds _this _args = return $ VNum 2015

dateSetTime :: JSFunction
dateSetTime _this _args = return $ VNum 22

dateSetFullYear :: JSFunction
dateSetFullYear _this _args = return $ VNum 2015

dateSetUTCFullYear :: JSFunction
dateSetUTCFullYear _this _args = return $ VNum 2015

dateSetMonth :: JSFunction
dateSetMonth _this _args = return $ VNum 2015

dateSetUTCMonth :: JSFunction
dateSetUTCMonth _this _args = return $ VNum 2015

dateSetDate :: JSFunction
dateSetDate _this _args = return $ VNum 2015

dateSetUTCDate :: JSFunction
dateSetUTCDate _this _args = return $ VNum 2015

dateSetDay :: JSFunction
dateSetDay _this _args = return $ VNum 2015

dateSetUTCDay :: JSFunction
dateSetUTCDay _this _args = return $ VNum 2015

dateSetHours :: JSFunction
dateSetHours _this _args = return $ VNum 2015

dateSetUTCHours :: JSFunction
dateSetUTCHours _this _args = return $ VNum 2015

dateSetMinutes :: JSFunction
dateSetMinutes _this _args = return $ VNum 2015

dateSetUTCMinutes :: JSFunction
dateSetUTCMinutes _this _args = return $ VNum 2015

dateSetSeconds :: JSFunction
dateSetSeconds _this _args = return $ VNum 2015

dateSetUTCSeconds :: JSFunction
dateSetUTCSeconds _this _args = return $ VNum 2015

dateSetMilliseconds :: JSFunction
dateSetMilliseconds _this _args = return $ VNum 2015

dateSetUTCMilliseconds :: JSFunction
dateSetUTCMilliseconds _this _args = return $ VNum 2015

dateToLocaleString :: JSFunction
dateToLocaleString _this _args = return $ VStr "xyzzy"

dateToUTCString :: JSFunction
dateToUTCString _this _args = return $ VStr "plugh"
