{-# Language LambdaCase #-}

module Runtime.Conversion where

import Data.Maybe
import Data.Word
import Data.Int
import Safe
import JSNum
import Runtime.Types
import Runtime.Object
import Parse


-- ref 9.1, incomplete
toPrimitive :: PrimitiveHint -> JSVal -> Runtime JSVal
toPrimitive hint a = case a of
  VObj obj -> objDefaultValue hint obj
  _        -> return a

-- ref 9.2, incomplete
toBoolean :: JSVal -> Bool
toBoolean VUndef    = False
toBoolean VNull     = False
toBoolean (VBool b) = b
toBoolean (VNum n)  = n /= 0 && not (isNaN $ fromJSNum n)
toBoolean (VStr "") = False
toBoolean _         = True

-- ref 9.3
toNumber :: JSVal -> Runtime JSNum
toNumber VUndef     = return jsNaN
toNumber VNull      = return 0
toNumber (VBool b)  = return $ if b then 1 else 0
toNumber (VNum n)   = return n
toNumber (VStr s)   = return $ strToNumber s
toNumber v@(VObj _) = toPrimitive HintNumber v >>= toNumber
toNumber _ = return 7

strToNumber :: String -> JSNum
strToNumber = parseNumber

isString :: JSVal -> Bool
isString (VStr _) = True
isString _ = False

showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) = numberToString n
showVal (VBool a) = if a then "true" else "false"
showVal (VRef ref) = "(reference " ++ show ref ++ ")"
showVal VUndef = "undefined"
showVal VNull  = "null"
showVal (VObj _) = "[Object object]"
showVal (VNative _) = "function"
showVal (VStacktrace st) = "Stacktrace " ++ show st
showVal other = show other

-- ref 9.5
toInt32 :: JSVal -> Runtime Int32
toInt32 = to32Bit

-- ref 9.6
toUInt32 :: JSVal -> Runtime Word32
toUInt32 = to32Bit

to32Bit :: Integral a => JSVal -> Runtime a
to32Bit val = do
  JSNum number <- toNumber val
  if number == 1/0 || number == -1/0 || number == 0 || number /= number
  then return 0
  else return $ sign number * floor (abs number)
    where sign a = floor (signum a)

-- ref 9.8
toString :: JSVal -> Runtime String
toString VUndef    = return "undefined"
toString VNull     = return "null"
toString (VBool b) = return $ if b then "true" else "false"
toString (VStr s)  = return s
toString (VNum n)  = return $ numberToString $ fromJSNum n
toString (VStacktrace st) = return $ unlines (map show st)
toString other = return $ show other

-- ref 9.10
checkObjectCoercible :: JSVal -> Runtime ()
checkObjectCoercible VUndef = raiseProtoError TypeError "Undefined value not coercible"
checkObjectCoercible VNull = raiseProtoError TypeError "Null value not coercible"
checkObjectCoercible _ = return ()

numberToString :: Double -> String
numberToString n
  | n == 0/0  = "NaN"
  | n == 1/0  = "Infinity"
  | n < 0     = "-" ++ numberToString (negate n)
  | isInteger n = show (round n :: Integer)
  | otherwise = show n

isInteger :: RealFloat a => a -> Bool
isInteger n = n == fromInteger (round n)

toObject :: JSVal -> Runtime (Shared JSObj)
toObject (VObj objRef) = return objRef
toObject (VStr str) = do
  obj <- newObject
  stringConstructor (VObj obj) [VStr str]
  return obj
toObject v = toString v >>= toObject . VStr

stringConstructor :: JSFunction
stringConstructor this args = do
  objRef <- toObj <$> getGlobalProperty "String"
  obj <- deref objRef
  case cstrMethod obj of
    Nothing -> raiseProtoError TypeError "Can't create string!"
    Just cstr -> cstr this args
