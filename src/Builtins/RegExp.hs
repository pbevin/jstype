{-# LANGUAGE LambdaCase #-}

module Builtins.RegExp where

import Control.Lens
import Control.Monad.Except
import Text.Regex.Posix
import Runtime
import Polyvariadic

makeRegExpClass :: Runtime (Shared JSObj)
makeRegExpClass = do
  regExpPrototype <- makePrototype "RegExp"
  updateObject regExpPrototype $ do
    method "exec"     1 regExpExec
    method "toString" 0 regExpToString
    native "test"     1 regExpTest

  functionPrototype <- findPrototypeForClass "Function"

  obj <- functionObject "RegExp" regExpPrototype
    >>= setCallMethod regExpFunction
    >>= setCstrMethod regExpConstructor
    >>= objSetPrototype functionPrototype
    >>= objSetHasInstance funHasInstance
    >>= addMethod "constructor" 1 regExpConstructor
    >>= addOwnProperty "length" (VNum 2)
    >>= addOwnProperty "prototype" (VObj regExpPrototype)

  addOwnProperty "constructor" (VObj obj) regExpPrototype

  return obj

-- ref 15.10.3.1
regExpFunction :: JSFunction
regExpFunction _this args =
  let (pattern, flags) = first2 args
  in do
    r <- getPrimitiveRegex pattern `catchError` const (return pattern)
    case (r, flags) of
      (VRegExp{}, VUndef) -> return pattern
      _                   -> do
        proto <- objFindPrototype "RegExp"
        this  <- newObject
           >>= objSetPrototype proto
        regExpConstructor (VObj this) args

-- ref 15.10.4.1
regExpConstructor :: JSVal -> [JSVal] -> Runtime JSVal
regExpConstructor this args =
  let (pattern, flags) = first2 args
  in case this of
    VObj objRef -> do
      obj <- objectWithClass "RegExp" pattern
      (p, f) <- case obj of
        Nothing -> makeRE pattern flags
        Just _ ->
          case flags of
            VUndef -> do
              v <- view objPrimitiveValue <$> deref (toObj pattern)
              case v of
                Just (VRegExp p f) -> return (p, parseFlags f)
                _                  -> makeRE pattern flags
            _ -> raiseTypeError ("copying RegExp with " ++ show flags)

      maybe (raiseSyntaxError "Bad RegExp flags") (initializeObject objRef p) f
    _ -> raiseTypeError "Bad type for RegExp constructor"

  where
    initializeObject :: Shared JSObj -> String -> RegExpFlags -> Runtime JSVal
    initializeObject obj p f = do
      proto <- objFindPrototype "RegExp"
      setClass "RegExp" obj
        >>= objSetPrimitive (VRegExp p $ flagsToString f)
        >>= addOwnProperty "prototype"  (VObj proto)
        >>= addOwnProperty "source"     (VStr p)
        >>= addOwnProperty "global"     (VBool $ global f)
        >>= addOwnProperty "ignoreCase" (VBool $ ignoreCase f)
        >>= addOwnProperty "multiline"  (VBool $ multiline f)
      return $ VObj obj

objectWithClass :: String -> JSVal -> Runtime (Maybe (Shared JSObj))
objectWithClass name val = case val of
  VObj obj -> do
    cls <- view objClass <$> deref obj
    if cls == name
    then return . Just $ obj
    else return Nothing
  _ -> return Nothing

makeRE :: JSVal -> JSVal -> Runtime (String, Maybe RegExpFlags)
makeRE pattern flags = do
  p <- toString pattern
  f <- toString flags
  return (p, parseFlags f)


regExpExec, regExpToString :: JSFunction
regExpExec _ _ = return VNull
regExpToString this _args = do
  VRegExp p f <- getPrimitiveRegex this
  return . VStr $ "/" ++ p ++ "/" ++ f

getPrimitiveRegex :: JSVal -> Runtime JSVal
getPrimitiveRegex v =
  case v of
    VObj o ->
      view objPrimitiveValue <$> deref o >>= \case
        Just r@(VRegExp{}) -> return r
        _                  -> raiseTypeError "Not a regexp"
    _ -> raiseTypeError "Not a regexp"

regExpTest :: JSVal -> String -> Bool
regExpTest this string = True





data RegExpFlags = RegExpFlags { global :: Bool, ignoreCase :: Bool, multiline :: Bool }
parseFlags :: String -> Maybe RegExpFlags
parseFlags flags = RegExpFlags <$> g <*> i <*> m
  where g = checkFor 'g'
        i = checkFor 'i'
        m = checkFor 'm'
        checkFor f = case length $ filter (== f) flags of
          0 -> Just False
          1 -> Just True
          _ -> Nothing

flagsToString :: RegExpFlags -> String
flagsToString f = map fst . filter snd $ [ ('g', global f), ('i', ignoreCase f), ('m', multiline f) ]
