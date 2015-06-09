module Builtins.String (makeStringClass) where

import Safe
import Runtime


makeStringClass :: Runtime (Shared JSObj)
makeStringClass = do
  stringPrototype <- newObject
    >>= setClass "String"
    >>= addOwnProperty "constructor" (VNative strConstructor)
    >>= addOwnProperty "toString" (VNative stringToString)
    >>= addOwnProperty "charAt" (VNative charAt)
    >>= addOwnProperty "charCodeAt" (VNative charCodeAt)
    >>= addOwnProperty "indexOf" (VNative indexOf)
    >>= addOwnProperty "lastIndexOf" (VNative lastIndexOf)
    >>= addOwnProperty "split" (VNative split)
    >>= addOwnProperty "substring" (VNative substring)
    >>= addOwnProperty "toLowerCase" (VNative toLowerCase)
    >>= addOwnProperty "toUpperCase" (VNative toUpperCase)

  functionObject "String" stringPrototype
    >>= setCallMethod strFunction
    >>= setCstrMethod (strConstructor)
    >>= addOwnProperty "fromCharCode" (VNative fromCharCode)

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
      str <- toString value
      setClass "String" obj >>= objSetPrototype prototype
                            >>= objSetExtensible True
                            >>= objSetPrimitive value
                            >>= addOwnProperty "length" (VNum $ fromIntegral $ length str)
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
fromCharCode _this _args = return VUndef
