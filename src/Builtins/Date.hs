module Builtins.Date (makeDateClass) where

import Runtime

makeDatePrototype :: Runtime (Shared JSObj)
makeDatePrototype =
  makePrototype "Date"
    >>= setCstrMethod dateConstructor
    >>= addOwnProperty "constructor" (        VNative 1 dateConstructor)
    >>= addOwnProperty "toString" (           VNative 0 dateToString)
    >>= addOwnProperty "valueOf" (            VNative 0 dateValueOf)
    >>= addOwnProperty "getTime" (            VNative 0 dateGetTime)
    >>= addOwnProperty "getFullYear" (        VNative 0 dateGetFullYear)
    >>= addOwnProperty "getUTCFullYear" (     VNative 0 dateGetUTCFullYear)
    >>= addOwnProperty "getMonth" (           VNative 0 dateGetMonth)
    >>= addOwnProperty "getUTCMonth" (        VNative 0 dateGetUTCMonth)
    >>= addOwnProperty "getDate" (            VNative 0 dateGetDate)
    >>= addOwnProperty "getUTCDate" (         VNative 0 dateGetUTCDate)
    >>= addOwnProperty "getDay" (             VNative 0 dateGetDay)
    >>= addOwnProperty "getUTCDay" (          VNative 0 dateGetUTCDay)
    >>= addOwnProperty "getHours" (           VNative 0 dateGetHours)
    >>= addOwnProperty "getUTCHours" (        VNative 0 dateGetUTCHours)
    >>= addOwnProperty "getMinutes" (         VNative 0 dateGetMinutes)
    >>= addOwnProperty "getUTCMinutes" (      VNative 0 dateGetUTCMinutes)
    >>= addOwnProperty "getSeconds" (         VNative 0 dateGetSeconds)
    >>= addOwnProperty "getUTCSeconds" (      VNative 0 dateGetUTCSeconds)
    >>= addOwnProperty "getMilliseconds" (    VNative 0 dateGetMilliseconds)
    >>= addOwnProperty "getUTCMilliseconds" ( VNative 0 dateGetUTCMilliseconds)
    >>= addOwnProperty "setTime" (            VNative 1 dateSetTime)
    >>= addOwnProperty "setFullYear" (        VNative 1 dateSetFullYear)
    >>= addOwnProperty "setUTCFullYear" (     VNative 1 dateSetUTCFullYear)
    >>= addOwnProperty "setMonth" (           VNative 1 dateSetMonth)
    >>= addOwnProperty "setUTCMonth" (        VNative 1 dateSetUTCMonth)
    >>= addOwnProperty "setDate" (            VNative 1 dateSetDate)
    >>= addOwnProperty "setUTCDate" (         VNative 1 dateSetUTCDate)
    >>= addOwnProperty "setDay" (             VNative 1 dateSetDay)
    >>= addOwnProperty "setUTCDay" (          VNative 1 dateSetUTCDay)
    >>= addOwnProperty "setHours" (           VNative 1 dateSetHours)
    >>= addOwnProperty "setUTCHours" (        VNative 1 dateSetUTCHours)
    >>= addOwnProperty "setMinutes" (         VNative 1 dateSetMinutes)
    >>= addOwnProperty "setUTCMinutes" (      VNative 1 dateSetUTCMinutes)
    >>= addOwnProperty "setSeconds" (         VNative 1 dateSetSeconds)
    >>= addOwnProperty "setUTCSeconds" (      VNative 1 dateSetUTCSeconds)
    >>= addOwnProperty "setMilliseconds" (    VNative 1 dateSetMilliseconds)
    >>= addOwnProperty "setUTCMilliseconds" ( VNative 1 dateSetUTCMilliseconds)
    >>= addOwnProperty "toLocaleString" (     VNative 0 dateToLocaleString)
    >>= addOwnProperty "toUTCString" (        VNative 0 dateToUTCString)


makeDateClass :: Runtime (Shared JSObj)
makeDateClass = do
  datePrototype <- makeDatePrototype

  functionObject "Date" datePrototype
    >>= setCallMethod dateFunction
    >>= setCstrMethod dateConstructor
    >>= addOwnProperty "prototype" (VObj datePrototype)
    >>= addOwnProperty "parse" (VNative 1 dateParse)
    >>= addOwnProperty "UTC" (VNative 1 dateUTC)

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
