{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Builtins.String where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char
import Safe
import Runtime
import Parse


makeStringClass :: Runtime (Shared JSObj)
makeStringClass = do
  stringPrototype <- newObject
    >>= setClass "String"
    >>= addMethod "constructor"  1 strConstructor
    >>= addMethod "toString"     0 stringToString
    >>= addMethod "charAt"       1 charAt
    >>= addMethod "charCodeAt"   1 charCodeAt
    >>= addMethod "indexOf"      1 indexOf
    >>= addMethod "lastIndexOf"  1 lastIndexOf
    >>= addMethod "split"        2 split
    >>= addMethod "substring"    2 substring
    >>= addMethod "toLowerCase"  0 toLowerCase
    >>= addMethod "toUpperCase"  0 toUpperCase
    >>= addMethod "replace"      2 replace

  functionObject "String" stringPrototype
    >>= setCallMethod strFunction
    >>= setCstrMethod (strConstructor)
    >>= addMethod "fromCharCode" 1 fromCharCode

-- ref 15.5.1.1
strFunction :: JSFunction
strFunction this args =
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
                            >>= objSetPrimitive value
                            >>= addOwnProperty "length" (VNum $ fromIntegral $ length str)
                            >>= addOwnProperty "constructor" stringObject
                            >>= setGetOwnPropertyMethod strGetOwnProperty
      return this

-- ref 15.5.4.2
stringToString :: JSFunction
stringToString this _args = do
  case this of
    VObj obj -> objGetPrimitive obj

-- ref 15.5.4.4, incomplete
charAt :: JSFunction
charAt this args =
  let pos = first1 args
  in do
    checkObjectCoercible "String.prototype.charAt called on" this
    str <- toString this
    position <- toInt pos
    return $ maybe VUndef charToStr $ atMay str position
      where charToStr ch = VStr [ch]

charCodeAt :: JSFunction
charCodeAt _this _args = return VUndef

indexOf :: JSFunction
indexOf _this _args = return VUndef

lastIndexOf :: JSFunction
lastIndexOf _this _args = return VUndef

split :: JSFunction
split _this _args = return VUndef

substring :: JSFunction
substring _this _args = return VUndef

toLowerCase :: JSFunction
toLowerCase _this _args = return VUndef

toUpperCase :: JSFunction
toUpperCase _this _args = return VUndef

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
        else let s = VStr [str !! index]
             in return $ Just $ dataPD s False False False

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

findOnce :: Text -> Text -> Maybe (Text, Text, Text)
findOnce needle haystack = case post of
  "" -> Nothing
  _  -> Just (pre, needle, T.drop (T.length needle) post)
  where (pre, post) = T.breakOn needle haystack
