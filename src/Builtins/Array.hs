module Builtins.Array (makeArrayClass) where

import Control.Monad
import Data.List (intercalate)
import Safe
import Data.Int
import Runtime
import Builtins.Array.Sort


makeArrayClass :: Runtime (Shared JSObj)
makeArrayClass = do
  arrayPrototype <- makePrototype "Array"
    >>= addOwnProperty "length" (VNum 0)
    >>= addMethod "toString" 0 arrayToString
    >>= addMethod "reduce" 1 arrayReduce
    >>= addMethod "join" 1 arrayJoin
    >>= addMethod "reverse" 0 arrayReverse
    >>= addMethod "sort" 1 arraySort

  functionObject "Array" arrayPrototype
    >>= setCallMethod (arrayFunction arrayPrototype)
    >>= setCstrMethod arrayConstructor
    >>= addOwnProperty "length" (VNum 0)
    >>= addMethod "isArray" 1 isArray


arrayFunction :: Shared JSObj -> JSFunction
arrayFunction prototype _this args = do
  obj <- newObject >>= setClass "Array"
                   >>= objSetPrototype prototype
  arrayConstructor (VObj obj) args

arrayConstructor :: JSFunction
arrayConstructor this args =
  let len = VNum $ fromIntegral $ length args
  in case this of
    VObj obj -> do
      cstr <- getGlobalProperty "Array"
      o <- setClass "Array" obj
        >>= addOwnPropertyDescriptor "constructor" (dataPD cstr True False True)
        >>= addOwnPropertyDescriptor "length" (dataPD len True False False)

      forM_ (zip args [0..]) $ \(item, idx) -> do
        addOwnProperty (show idx) item o

      return (VObj o)
      
    _ -> raiseError $ "arrayConstructor called with this = " ++ show this

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
      return $ VStr (intercalate sep d)

  where
    getString :: Show a => Shared JSObj -> a -> Runtime String
    getString o k = objGet (show k) o >>= stringOrEmpty
    stringOrEmpty :: JSVal -> Runtime String
    stringOrEmpty VUndef = return ""
    stringOrEmpty VNull  = return ""
    stringOrEmpty val    = toString val


-- ref 15.4.4.21, incomplete
arrayReduce :: JSVal -> [JSVal] -> Runtime JSVal
arrayReduce _this _args = return VNull

arrayReverse :: JSFunction
arrayReverse _this _args = return VUndef

isArray :: JSFunction
isArray _this _args = return (VBool True)
