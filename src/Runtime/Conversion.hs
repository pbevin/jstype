{-# Language LambdaCase, OverloadedStrings #-}

module Runtime.Conversion where

import Control.Lens
import Data.Maybe
import Data.Word
import Data.Int
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Safe
import Runtime.Types
import Runtime.Object
import Runtime.NumberToString
import Runtime.PropertyDescriptor
import Runtime.Shared
import JSNum
import Parse.Number (parseStrNum)


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
toBoolean (VNum n)  = n /= 0 && not (isNaN n)
toBoolean (VInt n)  = n /= 0
toBoolean (VStr "") = False
toBoolean _         = True

-- ref 9.3
toNumber :: JSVal -> Runtime Double
toNumber VUndef     = return jsNaN
toNumber VNull      = return 0
toNumber (VBool b)  = return $ if b then 1 else 0
toNumber (VNum n)   = return n
toNumber (VInt n)   = return $ fromIntegral n
toNumber (VStr s)   = return $ strToNumber s
toNumber v@(VObj _) = toPrimitive HintNumber v >>= toNumber
toNumber _ = return 7

toNum :: JSVal -> Runtime JSVal
toNum val = case val of
  VNum _  -> return val
  VInt _  -> return val
  VBool b -> return . VInt $ if b then 1 else 0
  VStr s  -> return (strToNum s)
  _       -> VNum <$> toNumber val

strToNumber :: Text -> Double
strToNumber str = case parseStrNum str of
                    Left i -> fromIntegral i
                    Right d -> d

strToNum :: Text -> JSVal
strToNum str = case parseStrNum str of
                 Left  int -> VInt int
                 Right dbl -> VNum dbl

isString :: JSVal -> Bool
isString (VStr _) = True
isString _ = False

showVal :: JSVal -> Text
showVal (VStr s) = s
showVal (VNum n) = T.pack $ numberToString n
showVal (VInt n) = T.pack $ show n
showVal (VBool a) = if a then "true" else "false"
showVal (VRef ref) = T.pack $ "(reference " ++ show ref ++ ")"
showVal VUndef = "undefined"
showVal VNull  = "null"
showVal (VObj _) = "[object Object]"
showVal (VNative n _ _) = "function/" <> n
showVal (VStacktrace st) = "Stacktrace " <> T.pack (show st)
showVal other = T.pack $ show other

toInt :: JSVal -> Runtime Int
toInt val = do
  makeInt <$> toNumber val
    -- where makeInt number
    --         | isNaN number      = 0
    --         | isInfinite number = 0
    --         | otherwise         = makeInt number


-- ref 9.5
toInt32 :: JSVal -> Runtime Int32
toInt32 = to32Bit

-- ref 9.6
toUInt32 :: JSVal -> Runtime Word32
toUInt32 = to32Bit

-- ref 9.7
toUInt16 :: JSVal -> Runtime Word16
toUInt16 = to32Bit

to32Bit :: Integral a => JSVal -> Runtime a
to32Bit val = do
  number <- toNumber val
  if number == 1/0 || number == -1/0 || number == 0 || number /= number
  then return 0
  else return $ makeInt number

makeInt :: Integral a => Double -> a
makeInt number = sign number * floor (abs number)
    where sign a = floor (signum a)

-- ref 9.8
toString :: JSVal -> Runtime Text
toString VUndef    = return "undefined"
toString VNull     = return "null"
toString (VBool b) = return $ if b then "true" else "false"
toString (VStr s)  = return s
toString (VNum n)  = return . T.pack $ numberToString n
toString (VInt n)  = return . T.pack $ show n
toString v@(VObj _) = toPrimitive HintString v >>= toString
toString (VStacktrace st) = return . T.pack $ unlines (map show st)
toString other = return . T.pack $ show other

-- ref 9.10
checkObjectCoercible :: String -> JSVal -> Runtime ()
checkObjectCoercible msg VUndef = raiseProtoError TypeError $ msg ++ " of undefined"
checkObjectCoercible msg VNull = raiseProtoError TypeError $ msg ++ " of null"
checkObjectCoercible _ _ = return ()


isInteger :: RealFloat a => a -> Bool
isInteger n = n == fromInteger (round n)

toObject :: JSVal -> Runtime (Shared JSObj)
toObject val = case val of
  VObj objRef   -> return objRef
  VStr _        -> wrapPrimitive "String" val
  VBool _       -> wrapPrimitive "Boolean" val
  VNum _        -> wrapPrimitive "Number" val
  VInt _        -> wrapPrimitive "Number" val
  VNative n l f -> wrapNative n l f
  VNull         -> raiseProtoError TypeError "null has no properties"
  VUndef        -> raiseProtoError TypeError "undefined has no properties"
  _             -> toString val >>= toObject . VStr

wrapPrimitive :: Text -> JSVal -> Runtime (Shared JSObj)
wrapPrimitive typeName val = do
  this <- newObject
  typeRef <- toObj <$> getGlobalProperty typeName
  obj <- deref typeRef
  case view cstrMethod obj of
    Nothing -> raiseProtoError TypeError . T.unpack $ "Can't create " <> typeName <> " object"
    Just cstr -> toObj <$> cstr (VObj this) [val]

wrapNative :: Text -> Int -> JSFunction -> Runtime (Shared JSObj)
wrapNative name len f = do
  funPrototype <- objFindPrototype "Function"
  newObject
    >>= addOwnPropDesc "length" (dataPD (VInt $ fromIntegral len) True True True)
    >>= addOwnProperty "call" (VNative name len $ nativeCall f)

nativeCall :: JSFunction -> JSFunction
nativeCall f this [] = nativeCall f this [VUndef]
nativeCall f _this (this:args) = f this args
