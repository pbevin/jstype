{-# Language LambdaCase #-}

module Runtime.Conversion where

import Data.Maybe
import Data.Word
import Data.Int
import Safe
import JSNum
import Runtime.Types
import Runtime.Object
import Runtime.NumberToString
import Runtime.PropertyDescriptor
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

to32Bit :: Integral a => JSVal -> Runtime a
to32Bit val = do
  number <- toNumber val
  if number == 1/0 || number == -1/0 || number == 0 || number /= number
  then return 0
  else return $ makeInt number

makeInt :: Integral a => JSNum -> a
makeInt number = sign number * floor (abs number)
    where sign a = floor (signum a)

-- ref 9.8
toString :: JSVal -> Runtime String
toString VUndef    = return "undefined"
toString VNull     = return "null"
toString (VBool b) = return $ if b then "true" else "false"
toString (VStr s)  = return s
toString (VNum n)  = return $ numberToString $ fromJSNum n
toString v@(VObj _) = toPrimitive HintString v >>= toString
toString (VStacktrace st) = return $ unlines (map show st)
toString other = return $ show other

-- ref 9.10
checkObjectCoercible :: String -> JSVal -> Runtime ()
checkObjectCoercible msg VUndef = raiseProtoError TypeError $ msg ++ " of undefined"
checkObjectCoercible msg VNull = raiseProtoError TypeError $ msg ++ " of null"
checkObjectCoercible _ _ = return ()


isInteger :: RealFloat a => a -> Bool
isInteger n = n == fromInteger (round n)

toObject :: JSVal -> Runtime (Shared JSObj)
toObject val = case val of
  VObj objRef -> return objRef
  VStr _      -> wrapPrimitive "String" val
  VBool _     -> wrapPrimitive "Boolean" val
  VNum _      -> wrapPrimitive "Number" val
  VNative f   -> wrapNative f
  _           -> toString val >>= toObject . VStr

wrapPrimitive :: String -> JSVal -> Runtime (Shared JSObj)
wrapPrimitive typeName val = do
  this <- newObject
  typeRef <- toObj <$> getGlobalProperty typeName
  obj <- deref typeRef
  case cstrMethod obj of
    Nothing -> raiseProtoError TypeError "Can't create string!"
    Just cstr -> toObj <$> cstr (VObj this) [val]

wrapNative :: JSFunction -> Runtime (Shared JSObj)
wrapNative f = do
  funPrototype <- objFindPrototype "Function"
  newObject
    >>= defineOwnProperty "length" (dataPD (VNum 3) True True True) False
    >>= addOwnProperty "call" (VNative $ nativeCall f)

nativeCall :: JSFunction -> JSFunction
nativeCall f this [] = nativeCall f this [VUndef]
nativeCall f _this (this:args) = f this args
