{-# LANGUAGE LambdaCase #-}

module Builtins.RegExp where

import Control.Monad.Except
import Text.Regex.PCRE.String
import Runtime

makeRegExpClass :: Runtime (Shared JSObj)
makeRegExpClass = do
  regExpPrototype <- makePrototype "RegExp"
    >>= addMethod      "exec"        1 regExpExec
    >>= addMethod      "test"        1 regExpTest
    >>= addMethod      "toString"    0 regExpToString

  functionPrototype <- findPrototypeForClass "Function"

  obj <- functionObject "RegExp" regExpPrototype
    >>= setCallMethod regExpFunction
    >>= setCstrMethod regExpConstructor
    >>= objSetPrototype functionPrototype
    >>= addMethod "constructor" 1 regExpConstructor
    >>= addOwnProperty "length" (VNum 2)
    >>= addOwnProperty "prototype" (VObj regExpPrototype)

  addOwnProperty "constructor" (VObj obj) regExpPrototype

  return obj

regExpFunction :: JSFunction
regExpFunction _this args =
  let (pattern, flags) = first2 args
  in do
    r <- getPrimitiveRegex pattern `catchError` const (return pattern)
    case (r, flags) of
      (VRegExp{}, VUndef) -> return pattern
      otherwise -> do
        this <- newObject
        regExpConstructor (VObj this) args

regExpConstructor :: JSVal -> [JSVal] -> Runtime JSVal
regExpConstructor this args =
  let (pattern, flags) = first2 args
  in case this of

    VObj objRef -> do
      p <- toString pattern
      liftIO (compile 0 0 p) >>= \case
        Left (_offset, err) -> raiseSyntaxError err
        Right re -> do
          prototype <- objFindPrototype "RegExp"
          setClass "RegExp" objRef
            >>= addOwnProperty "prototype" (VObj prototype)
            >>= objSetPrimitive (VRegExp p "" re)

          return $ VObj objRef
    _ -> raiseTypeError "Bad type for RegExp constructor"

regExpExec, regExpTest, regExpToString :: JSFunction
regExpExec = error "regExpExec"
regExpTest = error "regExpTest"
regExpToString this args = do
  VRegExp p f _ <- getPrimitiveRegex this
  return . VStr $ "/" ++ p ++ "/"

getPrimitiveRegex :: JSVal -> Runtime JSVal
getPrimitiveRegex v =
  case v of
    VObj o ->
      objPrimitiveValue <$> deref o >>= \case
        Just v@(VRegExp{}) -> return v
        otherwise          -> raiseTypeError "Not a regexp"
    _ -> raiseTypeError "Not a regexp"
