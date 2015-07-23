{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Builtins.String where

import Prelude hiding (error)
import Control.Lens hiding (pre, index)
import Text.Regex.Posix ((=~), MatchOffset, MatchLength)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set as S
import Data.Maybe
import Data.Char
import Safe
import Runtime
import Parse
import Builtins.RegExp
import Polyvariadic


makeStringClass :: Runtime (Shared JSObj)
makeStringClass = do
  stringPrototype <- mkObject $ do
    className "String"
    method "constructor"  1 strConstructor
    method "toString"     0 stringToString
    method "valueOf"      0 stringValueOf
    method "charAt"       1 charAt
    method "charCodeAt"   1 charCodeAt
    method "indexOf"      1 indexOf
    method "lastIndexOf"  1 lastIndexOf
    method "substring"    2 substring
    method "toLocaleLowerCase"  0 toLowerCase
    method "toLocaleUpperCase"  0 toUpperCase
    method "toLowerCase"  0 toLowerCase
    method "toUpperCase"  0 toUpperCase
    method "replace"      2 replace
    method "search"       1 search
    method "trim"         0 trim
    native "split"        2 split

  functionPrototype <- objFindPrototype "Function"

  newObject
    >>= setClass "String"
    >>= objSetPrototype functionPrototype
    >>= setCallMethod strFunction
    >>= setCstrMethod strConstructor
    >>= objSetHasInstance funHasInstance
    >>= addMethod "fromCharCode" 1 fromCharCode
    >>= addOwnConstant "length" (VInt 1)
    >>= addOwnConstant "prototype" (VObj stringPrototype)

-- ref 15.5.1.1
strFunction :: JSFunction
strFunction _this args =
  let value = firstArg (VStr "") args
  in VStr <$> toString value

-- ref 15.5.2.1
strConstructor :: JSFunction
strConstructor this args =
  let value = firstArg  (VStr "") args
  in case this of
    VObj obj -> do
      proto <- objFindPrototype "String"
      stringObject <- getGlobalProperty "String"
      str <- toString value
      setClass "String" obj >>= objSetPrototype proto
                            >>= objSetExtensible True
                            >>= objSetPrimitive (VStr str)
                            >>= addOwnConstant "length" (VInt $ fromIntegral $ T.length str)
                            >>= addOwnProperty "constructor" stringObject
                            >>= setGetOwnPropertyMethod strGetOwnProperty
      return this
    _ -> raiseTypeError "Bad type for String constructor"

-- ref 15.5.4.2
stringToString :: JSFunction
stringToString this _args = do
  maybe error return =<< case this of
   VStr _   -> return (Just this)
   VObj obj -> view objPrimitiveValue <$> deref obj >>= \case
     Just (VStr s) -> return . Just . VStr $ s
     _             -> return Nothing
   _ -> return Nothing
  where error = raiseTypeError "Bad type for String constructor"

-- ref 15.5.4.3
stringValueOf :: JSFunction
stringValueOf = stringToString

-- ref 15.5.4.4
charAt :: JSFunction
charAt = charAtMethod "charAt" (VStr . maybe "" T.singleton)

-- ref 15.5.4.5
charCodeAt :: JSFunction
charCodeAt = charAtMethod "charCodeAt" $ maybe (VNum jsNaN) (VInt . fromIntegral . ord)

charAtMethod :: String -> (Maybe Char -> JSVal) -> JSFunction
charAtMethod name f this args =
  let pos = first1 args
  in do
    checkObjectCoercible ("String.prototype." ++ name ++ " called on") this
    str <- toString this
    position <- toInt pos
    if 0 <= position && position < T.length str
    then return . f . Just $ str `T.index` position
    else return . f $ Nothing

-- ref 15.5.4.7
indexOf :: JSFunction
indexOf this args = do
  let (searchStr, position) = first2 args

  checkObjectCoercible "Cannot get string value" this
  s               <- toString this
  searchString    <- toString searchStr
  pos             <- toInt position
  let len          = T.length s
      start        = min (max pos 0) len
      searchLen    = T.length searchString
      needle       = searchString
      haystack     = T.drop start s
      (pre, match) = T.breakOn needle haystack
      index        = if match == "" then -1 else start + T.length pre
  return . VInt . fromIntegral $ index


lastIndexOf :: JSFunction
lastIndexOf this args = do
  let (searchStr, position) = first2 args

  checkObjectCoercible "Cannot get string value" this
  s               <- toString this
  searchString    <- toString searchStr
  pos0            <- toNumber position
  let len          = T.length s
      pos          = if isNaN pos0 then len + 1 else round pos0
      start        = min (max pos 0) len
      searchLen    = T.length searchString
      needle       = searchString
      haystack     = T.take start s
      (pre, match) = T.breakOnEnd needle haystack
      index        = if pre == "" then -1 else T.length pre - T.length needle
  return . VInt . fromIntegral $ index

split :: Text -> Maybe Text -> Maybe Int -> [Text]
split str (Just sep) Nothing = T.splitOn sep str

substring :: JSFunction
substring _this _args = return VUndef

-- ref 15.5.4.16
toLowerCase :: JSFunction
toLowerCase = withT T.toLower

-- ref 15.5.4.18
toUpperCase :: JSFunction
toUpperCase = withT T.toUpper

withT :: (T.Text -> T.Text) -> JSFunction
withT f this _args = do
  checkObjectCoercible "Cannot get string value" this
  VStr . f <$> toString this

fromCharCode :: JSFunction
fromCharCode _this args = VStr . T.pack . map (chr . fromIntegral) <$> mapM toUInt16 args

strGetOwnProperty :: Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
strGetOwnProperty p s = do
  objGetOwnPropertyObj p s >>= \case
    Just d -> return (Just d)
    Nothing -> do
      index <- toInt (VStr p)
      let p' = T.pack $ show (abs index)
      if p /= p'
      then return Nothing
      else do
        VStr str <- objGetPrimitive s
        if index < 0 || index >= T.length str
        then return Nothing
        else let ch = VStr . T.singleton $ str `T.index` index
             in return $ Just $ dataPD ch False False False

replace :: JSFunction
replace this args =
  let (searchValue, replaceValue) = first2 args
  in do
    checkObjectCoercible "Cannot get string value" this
    string <- toString this
    searchString <- toString searchValue
    case findOnce searchString string of
      Nothing -> return this
      Just (before, match, after) -> do
        isCallable replaceValue >>= \case
          Nothing -> raiseTypeError "Replace value is not a function"
          Just call -> do
            let offset = T.length before
            result <- call VUndef [VStr $ match, VInt (fromIntegral offset), VStr string]
            replacement <- toString result
            return . VStr $ T.concat [before, replacement, after]

  where
    findOnce :: Text -> Text -> Maybe (Text, Text, Text)
    findOnce needle haystack = case post of
      "" -> Nothing
      _  -> Just (pre, needle, T.drop (T.length needle) post)
      where (pre, post) = T.breakOn needle haystack

-- ref 15.5.4.12
search :: JSFunction
search this args = do
  let regexp = first1 args
  string <- toString this

  o <- toObject =<< regExpFunction VUndef [regexp]
  view objPrimitiveValue <$> (deref o) >>= \case
    Just (VRegExp p _f) -> do
      let (offset, _) = (T.unpack string) =~ (T.unpack p) :: (MatchOffset, MatchLength)
      return (VInt . fromIntegral $ offset)
    _ -> raiseSyntaxError "No regexp found"


-- ref 15.5.4.20
trim :: JSFunction
trim this _args = do
  checkObjectCoercible "Cannot call trim() method" this
  VStr . T.dropAround isJsSpace <$> toString this
