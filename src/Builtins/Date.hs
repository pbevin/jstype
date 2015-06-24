module Builtins.Date (makeDateClass) where

import Runtime

makeDatePrototype :: Runtime (Shared JSObj)
makeDatePrototype = do
  objectPrototype <- objFindPrototype "Object"

  mkObject $ do
    className "Date"
    prototype objectPrototype
    internal cstrMethod dateConstructor
    method "toString"            0 dateToString
    method "valueOf"             0 dateValueOf
    method "getTime"             0 dateGetTime
    method "getFullYear"         0 dateGetFullYear
    method "getUTCFullYear"      0 dateGetUTCFullYear
    method "getMonth"            0 dateGetMonth
    method "getUTCMonth"         0 dateGetUTCMonth
    method "getDate"             0 dateGetDate
    method "getUTCDate"          0 dateGetUTCDate
    method "getDay"              0 dateGetDay
    method "getUTCDay"           0 dateGetUTCDay
    method "getHours"            0 dateGetHours
    method "getUTCHours"         0 dateGetUTCHours
    method "getMinutes"          0 dateGetMinutes
    method "getUTCMinutes"       0 dateGetUTCMinutes
    method "getSeconds"          0 dateGetSeconds
    method "getUTCSeconds"       0 dateGetUTCSeconds
    method "getMilliseconds"     0 dateGetMilliseconds
    method "getUTCMilliseconds"  0 dateGetUTCMilliseconds
    method "setTime"             1 dateSetTime
    method "setFullYear"         1 dateSetFullYear
    method "setUTCFullYear"      1 dateSetUTCFullYear
    method "setMonth"            1 dateSetMonth
    method "setUTCMonth"         1 dateSetUTCMonth
    method "setDate"             1 dateSetDate
    method "setUTCDate"          1 dateSetUTCDate
    method "setDay"              1 dateSetDay
    method "setUTCDay"           1 dateSetUTCDay
    method "setHours"            1 dateSetHours
    method "setUTCHours"         1 dateSetUTCHours
    method "setMinutes"          1 dateSetMinutes
    method "setUTCMinutes"       1 dateSetUTCMinutes
    method "setSeconds"          1 dateSetSeconds
    method "setUTCSeconds"       1 dateSetUTCSeconds
    method "setMilliseconds"     1 dateSetMilliseconds
    method "setUTCMilliseconds"  1 dateSetUTCMilliseconds
    method "toLocaleString"      0 dateToLocaleString
    method "toUTCString"         0 dateToUTCString


makeDateClass :: Runtime (Shared JSObj)
makeDateClass = do
  functionPrototype <- objFindPrototype "Function"
  datePrototype     <- makeDatePrototype

  obj <- mkObject $ do
    className "Function"
    prototype functionPrototype
    property "prototype" (VObj datePrototype)
    internal callMethod dateFunction
    internal cstrMethod dateConstructor
    internal hasInstanceMethod funHasInstance
    method "parse" 1 dateParse
    method "UTC"   1 dateUTC

  addOwnProperty "constructor" (VObj obj) datePrototype
  return obj

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
