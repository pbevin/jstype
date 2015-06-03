module Builtins.Array (makeArrayClass) where

import Safe
import Runtime


makeArrayClass :: Runtime (Shared JSObj)
makeArrayClass = do
  arrayPrototype <- makePrototype "Array"
    >>= addOwnProperty "constructor" (VNative arrayConstructor)
    >>= addOwnProperty "length" (VNum 0)
    >>= addOwnProperty "toString" (VNative arrayToString)
    >>= addOwnProperty "reduce" (VNative arrayReduce)
    >>= addOwnProperty "join" (VNative arrayJoin)
    >>= addOwnProperty "reverse" (VNative arrayReverse)
    >>= addOwnProperty "sort" (VNative arraySort)

  functionObject "Array" arrayPrototype
    >>= setCallMethod (arrayFunction arrayPrototype)
    >>= setCstrMethod arrayConstructor
    >>= addOwnProperty "length" (VNum 0)
    >>= addOwnProperty "isArray" (VNative isArray)


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
      VObj <$> (setClass "Array" obj
                  >>= addOwnProperty "length" len
                  >>= addOwnProperty "constructor" cstr)
    _ -> raiseError $ "arrayConstructor called with this = " ++ show this

-- ref 15.4.4.2, incomplete
arrayToString :: JSVal -> [JSVal] -> Runtime JSVal
arrayToString _this _args = return $ VStr "[...]"

-- ref 15.4.4.21, incomplete
arrayReduce :: JSVal -> [JSVal] -> Runtime JSVal
arrayReduce _this _args = return VNull

arrayJoin :: JSFunction
arrayJoin _this _args = return VUndef

arrayReverse :: JSFunction
arrayReverse _this _args = return VUndef

arraySort :: JSFunction
arraySort _this _args = return VUndef

isArray :: JSFunction
isArray _this _args = return (VBool True)
