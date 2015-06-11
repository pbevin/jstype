module Builtins.Date (makeDateClass) where

import Runtime

makeDatePrototype :: Runtime (Shared JSObj)
makeDatePrototype =
  makePrototype "Date"
    >>= setCstrMethod dateConstructor
    >>= addMethod "constructor"         1 dateConstructor
    >>= addMethod "toString"            0 dateToString
    >>= addMethod "valueOf"             0 dateValueOf
    >>= addMethod "getTime"             0 dateGetTime
    >>= addMethod "getFullYear"         0 dateGetFullYear
    >>= addMethod "getUTCFullYear"      0 dateGetUTCFullYear
    >>= addMethod "getMonth"            0 dateGetMonth
    >>= addMethod "getUTCMonth"         0 dateGetUTCMonth
    >>= addMethod "getDate"             0 dateGetDate
    >>= addMethod "getUTCDate"          0 dateGetUTCDate
    >>= addMethod "getDay"              0 dateGetDay
    >>= addMethod "getUTCDay"           0 dateGetUTCDay
    >>= addMethod "getHours"            0 dateGetHours
    >>= addMethod "getUTCHours"         0 dateGetUTCHours
    >>= addMethod "getMinutes"          0 dateGetMinutes
    >>= addMethod "getUTCMinutes"       0 dateGetUTCMinutes
    >>= addMethod "getSeconds"          0 dateGetSeconds
    >>= addMethod "getUTCSeconds"       0 dateGetUTCSeconds
    >>= addMethod "getMilliseconds"     0 dateGetMilliseconds
    >>= addMethod "getUTCMilliseconds"  0 dateGetUTCMilliseconds
    >>= addMethod "setTime"             1 dateSetTime
    >>= addMethod "setFullYear"         1 dateSetFullYear
    >>= addMethod "setUTCFullYear"      1 dateSetUTCFullYear
    >>= addMethod "setMonth"            1 dateSetMonth
    >>= addMethod "setUTCMonth"         1 dateSetUTCMonth
    >>= addMethod "setDate"             1 dateSetDate
    >>= addMethod "setUTCDate"          1 dateSetUTCDate
    >>= addMethod "setDay"              1 dateSetDay
    >>= addMethod "setUTCDay"           1 dateSetUTCDay
    >>= addMethod "setHours"            1 dateSetHours
    >>= addMethod "setUTCHours"         1 dateSetUTCHours
    >>= addMethod "setMinutes"          1 dateSetMinutes
    >>= addMethod "setUTCMinutes"       1 dateSetUTCMinutes
    >>= addMethod "setSeconds"          1 dateSetSeconds
    >>= addMethod "setUTCSeconds"       1 dateSetUTCSeconds
    >>= addMethod "setMilliseconds"     1 dateSetMilliseconds
    >>= addMethod "setUTCMilliseconds"  1 dateSetUTCMilliseconds
    >>= addMethod "toLocaleString"      0 dateToLocaleString
    >>= addMethod "toUTCString"         0 dateToUTCString


makeDateClass :: Runtime (Shared JSObj)
makeDateClass = do
  datePrototype <- makeDatePrototype

  functionObject "Date" datePrototype
    >>= setCallMethod dateFunction
    >>= setCstrMethod dateConstructor
    >>= addOwnProperty "prototype" (VObj datePrototype)
    >>= addMethod      "parse"     1 dateParse
    >>= addMethod      "UTC"       1 dateUTC

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
