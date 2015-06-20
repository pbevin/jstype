{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Builtins.String where

import Text.Regex.Posix ((=~), MatchOffset, MatchLength)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set as S
import Data.Maybe
import Data.Char hiding (isSpace)
import Safe
import Runtime
import Parse
import Builtins.RegExp


makeStringClass :: Runtime (Shared JSObj)
makeStringClass = do
  stringPrototype <- newObject
    >>= setClass "String"
    >>= addMethod "constructor"  1 strConstructor
    >>= addMethod "toString"     0 stringToString
    >>= addMethod "valueOf"      0 stringValueOf
    >>= addMethod "charAt"       1 charAt
    >>= addMethod "charCodeAt"   1 charCodeAt
    >>= addMethod "indexOf"      1 indexOf
    >>= addMethod "lastIndexOf"  1 lastIndexOf
    >>= addMethod "split"        2 split
    >>= addMethod "substring"    2 substring
    >>= addMethod "toLocaleLowerCase"  0 toLowerCase
    >>= addMethod "toLocaleUpperCase"  0 toUpperCase
    >>= addMethod "toLowerCase"  0 toLowerCase
    >>= addMethod "toUpperCase"  0 toUpperCase
    >>= addMethod "replace"      2 replace
    >>= addMethod "search"       1 search
    >>= addMethod "trim"         0 trim

  functionPrototype <- objFindPrototype "Function"

  newObject
    >>= setClass "String"
    >>= objSetPrototype functionPrototype
    >>= setCallMethod strFunction
    >>= setCstrMethod (strConstructor)
    >>= addMethod "fromCharCode" 1 fromCharCode
    >>= addOwnConstant "length" (VNum 1)
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
      prototype <- objFindPrototype "String"
      stringObject <- getGlobalProperty "String"
      str <- toString value
      setClass "String" obj >>= objSetPrototype prototype
                            >>= objSetExtensible True
                            >>= objSetPrimitive (VStr str)
                            >>= addOwnConstant "length" (VNum $ fromIntegral $ length str)
                            >>= addOwnProperty "constructor" stringObject
                            >>= setGetOwnPropertyMethod strGetOwnProperty
      return this
    _ -> raiseTypeError "Bad type for String constructor"

-- ref 15.5.4.2
stringToString :: JSFunction
stringToString this _args = do
  maybe error return =<< case this of
   VStr _   -> return (Just this)
   VObj obj -> objPrimitiveValue <$> deref obj >>= \case
     Just (VStr s) -> return . Just . VStr $ s
     _             -> return Nothing
   _ -> return Nothing
  where error = raiseTypeError "Bad type for String constructor"

-- ref 15.5.4.3
stringValueOf :: JSFunction
stringValueOf = stringToString

-- ref 15.5.4.4
charAt :: JSFunction
charAt = charAtMethod "charAt" (VStr . maybe "" (replicate 1))

-- ref 15.5.4.5
charCodeAt :: JSFunction
charCodeAt = charAtMethod "charCodeAt" $ maybe (VNum jsNaN) (VNum . fromIntegral . ord)

charAtMethod :: String -> (Maybe Char -> JSVal) -> JSFunction
charAtMethod name f this args =
  let pos = first1 args
  in do
    checkObjectCoercible ("String.prototype." ++ name ++ " called on") this
    str <- toString this
    position <- toInt pos
    return . f . atMay str $ position

-- ref 15.5.4.7
indexOf :: JSFunction
indexOf this args = do
  let (searchStr, position) = first2 args

  checkObjectCoercible "Cannot get string value" this
  s               <- toString this
  searchString    <- toString searchStr
  pos             <- toInt position
  let len          = length s
      start        = min (max pos 0) len
      searchLen    = length searchString
      needle       = T.pack searchString
      haystack     = T.drop start (T.pack s)
      (pre, match) = T.breakOn needle haystack
      index        = if match == "" then -1 else start + T.length pre
  return . VNum . fromIntegral $ index


lastIndexOf :: JSFunction
lastIndexOf this args = do
  let (searchStr, position) = first2 args

  checkObjectCoercible "Cannot get string value" this
  s               <- toString this
  searchString    <- toString searchStr
  pos0            <- toNumber position
  let len          = length s
      pos          = if isJsNaN pos0 then len + 1 else jsToInt pos0
      start        = min (max pos 0) len
      searchLen    = length searchString
      needle       = T.pack searchString
      haystack     = T.take start (T.pack s)
      (pre, match) = T.breakOnEnd needle haystack
      index        = if pre == "" then -1 else T.length pre - T.length needle
  return . VNum . fromIntegral $ index

split :: JSFunction
split _this _args = return VUndef

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
  s <- toString this
  return . VStr . T.unpack . f . T.pack $ s

fromCharCode :: JSFunction
fromCharCode _this args = VStr . map (chr . fromIntegral) <$> mapM toUInt16 args

strGetOwnProperty :: String -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
strGetOwnProperty p s = do
  objGetOwnPropertyObj p s >>= \case
    Just d -> return (Just d)
    Nothing -> do
      index <- toInt (VStr p)
      let p' = show (abs index)
      if p /= p'
      then return Nothing
      else do
        VStr str <- objGetPrimitive s
        if length str <= index
        then return Nothing
        else let ch = VStr [str !! index]
             in return $ Just $ dataPD ch False False False

replace :: JSFunction
replace this args =
  let (searchValue, replaceValue) = first2 args
  in do
    checkObjectCoercible "Cannot get string value" this
    string <- toString this
    searchString <- toString searchValue
    case findOnce (T.pack searchString) (T.pack string) of
      Nothing -> return this
      Just (before, match, after) -> do
        isCallable replaceValue >>= \case
          Nothing -> raiseTypeError "Replace value is not a function"
          Just call -> do
            let offset = T.length before
            result <- call VUndef [VStr $ T.unpack match, VNum (fromIntegral offset), VStr string]
            replacement <- toString result
            return . VStr . T.unpack $ T.concat [before, T.pack replacement, after]

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
  objPrimitiveValue <$> (deref o) >>= \case
    Just (VRegExp p _f) -> do
      let (offset, _) = string =~ p :: (MatchOffset, MatchLength)
      return (VNum . fromIntegral $ offset)
    _ -> raiseSyntaxError "No regexp found"


-- ref 15.5.4.20
trim :: JSFunction
trim this _args = do
  checkObjectCoercible "Cannot call trim() method" this
  s <- toString this
  return . VStr . T.unpack . T.dropAround isSpace . T.pack $ s


spaceChars :: S.Set Char
spaceChars = S.fromList (lineBreaks ++ iso8859Spaces ++ unicodeSpaces)
  where lineBreaks    = [ '\n', '\r', '\x2028', '\x2029' ]
        iso8859Spaces = [ '\t', '\v', '\f', ' ', '\xa0'  ]
        unicodeSpaces = map chr [ 0x2000..0x200a ] ++
                        [ '\x1680', '\x180e', '\x202f' ] ++
                        [ '\x205f', '\x3000', '\xfeff' ]

isSpace :: Char -> Bool
isSpace ch = ch `S.member` spaceChars
