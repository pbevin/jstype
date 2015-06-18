module Builtins.RegExp where

import Runtime

makeRegExpClass :: Runtime (Shared JSObj)
makeRegExpClass = do
  regExpPrototype <- makePrototype "RegExp"
    >>= addMethod "constructor" 1 regExpConstructor
    >>= addMethod "exec"        1 regExpExec
    >>= addMethod "test"        1 regExpTest
    >>= addMethod "toString" 0 regExpToString

  functionPrototype <- findPrototypeForClass "Function"

  obj <- functionObject "RegExp" regExpPrototype
    >>= setCallMethod regExpFunction
    >>= setCstrMethod regExpConstructor
    >>= objSetPrototype functionPrototype
    >>= addMethod "constructor" 1 regExpConstructor
    >>= addOwnProperty "length" (VNum 2)
    >>= addOwnProperty "prototype" (VObj regExpPrototype)

  return obj



functionIsConstructor :: JSFunction -> Shared JSObj -> JSFunction
functionIsConstructor cstr proto _this args = do
  this <- newObject >>= objSetPrototype proto
  cstr (VObj this) args

regExpFunction :: JSFunction
regExpFunction _this args = do
  this <- newObject
  regExpConstructor (VObj this) args


regExpConstructor :: JSVal -> [JSVal] -> Runtime JSVal
regExpConstructor this _ = case this of
  VObj objRef -> VObj <$> (setClass "RegExp" objRef)
  _ -> raiseTypeError "Bad type for RegExp constructor"

regExpExec, regExpTest, regExpToString :: JSFunction
regExpExec = undefined
regExpTest = undefined
regExpToString = undefined
