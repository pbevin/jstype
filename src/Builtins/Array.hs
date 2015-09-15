{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Builtins.Array (makeArrayClass) where

import Control.Monad
import Control.Lens
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Data.Char (isDigit)
import Safe
import Data.Int
import Runtime
import Builtins.Array.Reduce
import Builtins.Array.Sort
import Builtins.Array.Concat


makeArrayClass :: Runtime (Shared JSObj)
makeArrayClass = do
  objectPrototype <- getGlobalObjectPrototype

  arrayPrototype <- mkObject $ do
    className "Array"
    prototype objectPrototype

    property "length" (VInt 0)

    method "toString" 0 arrayToString
    method "reduce"   1 arrayReduce
    method "join"     1 arrayJoin
    method "reverse"  0 arrayReverse
    method "sort"     1 arraySort
    method "push"     1 arrayPush
    method "concat"   1 arrayConcat

  obj <- mkObject $ do
    isFunctionObject
    internal callMethod (arrayFunction arrayPrototype)
    internal cstrMethod arrayConstructor
    internal hasInstanceMethod funHasInstance

    property "name" (VStr "Array")
    property "prototype" (VObj arrayPrototype)
    property "length" (VInt 0)
    method   "isArray" 1 isArray

  addOwnProperty "constructor" (VObj obj) arrayPrototype
  return obj


arrayFunction :: Shared JSObj -> JSFunction
arrayFunction proto _this args = do
  obj <- newObject >>= setClass "Array"
                   >>= objSetPrototype proto
  arrayConstructor (VObj obj) args

arrayConstructor :: JSFunction
arrayConstructor this args =
  case (this, args) of
    (VObj obj, [len]) -> VObj <$> arrayConstructorLength obj len
    (VObj obj, _)     -> VObj <$> arrayConstructorElements obj args
    _ -> raiseError . T.pack $ "arrayConstructor called with this = " ++ show this

arrayConstructorLength :: Shared JSObj -> JSVal -> Runtime (Shared JSObj)
arrayConstructorLength obj arrayLen = do
  len <- VInt . fromIntegral <$> toInt arrayLen
  cstr <- getGlobalProperty "Array"
  o <- setClass "Array" obj
    >>= addOwnPropertyDescriptor "constructor" (dataPD cstr True False True)
    >>= addOwnPropertyDescriptor "length" (dataPD len True False False)
  return o

arrayConstructorElements :: Shared JSObj -> [JSVal] -> Runtime (Shared JSObj)
arrayConstructorElements obj args = do
  let len = VInt $ fromIntegral $ length args
  o <- arrayConstructorLength obj len
         >>= setDefineOwnPropertyMethod arrayDefineOwnProperty

  forM_ (zip args [0..]) $ \(item, idx) -> do
    addOwnProperty (T.pack $ show idx) item o

  return o

-- ref 15.4.4.2, incomplete
arrayToString :: JSVal -> [JSVal] -> Runtime JSVal
arrayToString this _args = arrayJoin this []

-- ref 15.4.4.5, incomplete
arrayJoin :: JSFunction
arrayJoin this args =
  let separator = first1 args
  in do
    o <- toObject this
    lenVal <- objGet "length" o
    len <- toInt32 lenVal
    sep <- toString $ ifUndefined (VStr ",") separator
    if len == 0
    then return (VStr "")
    else do
      d <- forM [0..len-1] $ getString o
      return $ VStr (T.intercalate sep d)

  where
    getString :: Show a => Shared JSObj -> a -> Runtime Text
    getString o k = objGet (T.pack $ show k) o >>= stringOrEmpty
    stringOrEmpty :: JSVal -> Runtime Text
    stringOrEmpty VUndef = return ""
    stringOrEmpty VNull  = return ""
    stringOrEmpty val    = toString val


arrayReverse :: JSFunction
arrayReverse _this _args = return VUndef

isArray :: JSFunction
isArray _this _args = return (VBool True)

arrayPush :: JSFunction
arrayPush this args = do
  obj <- toObject this
  lenVal <- objGet "length" obj
  n <- toInt lenVal
  n' <- VInt . fromIntegral <$> placeValues obj n args
  objPut "length" n' True obj
  return n'

  where
    placeValues :: Shared JSObj -> Int -> [JSVal] -> Runtime Int
    placeValues   _ n []     = return n
    placeValues obj n (e:es) = objPut (T.pack $ show n) e True obj >> placeValues obj (n+1) es

-- ref 15.4.5.1
arrayDefineOwnProperty :: Text -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool
arrayDefineOwnProperty p desc throw a
  | p == "length"  = error "array length unimplemented"
  | isArrayIndex p = setIndexOnArray p desc throw a
  | otherwise      = objDefineOwnPropertyObject p desc throw a

isArrayIndex :: Text -> Bool
isArrayIndex p
  | T.all isDigit p = let n = readInt32 p in n /= 2^32 - 1 && p == T.pack (show n)
  | otherwise       = False

readInt32 :: Text -> Integer
readInt32 p =
  let n = read . T.unpack $ p
   in n `mod` 2^32

setIndexOnArray :: Text -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool
setIndexOnArray p desc throw a = do
  Just oldLenDesc <- objGetOwnProperty "length" a
  let Just (VInt oldLen) = propValue oldLenDesc
      n = readInt32 p
  -- reject if n >= oldLen and oldLen not writable
  objDefineOwnPropertyObject p desc throw a
  if n >= oldLen
     then objDefineOwnPropertyObject "length" (setValue (VInt $ n+1) oldLenDesc) throw a
     else return False
