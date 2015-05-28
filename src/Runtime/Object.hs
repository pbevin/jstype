module Runtime.Object where

import Data.Functor
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Runtime.Types
import Runtime.Global
import Runtime.PropMap

import Debug.Trace

newObject :: Runtime (Shared JSObj)
newObject = do
  prototype <- getGlobalObjectPrototype
  share JSObj { objClass = "Object",
                ownProperties = emptyPropMap,
                objPrototype = Just prototype,
                callMethod = Nothing,
                cstrMethod = Nothing }

-- ref 8.12.1
objGetOwnProperty :: String -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
objGetOwnProperty name objRef = liftM (propMapLookup name . ownProperties) (deref objRef)

-- ref 8.12.2
objGetProperty :: String -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
objGetProperty name objRef = do
  prop <- objGetOwnProperty name objRef
  case prop of
    Just _ -> return prop
    Nothing -> do
      proto <- liftM objPrototype $ deref objRef
      case proto of
        Just p -> objGetProperty name p
        Nothing -> return Nothing

valGetProperty :: String -> JSVal -> Runtime (Maybe (PropDesc JSVal))
valGetProperty name (VObj objRef) = objGetProperty name objRef
valGetProperty _ _ = return Nothing

-- ref 8.12.3, incomplete
objGet :: String -> Shared JSObj -> Runtime JSVal
objGet name objRef = do
  prop <- objGetProperty name objRef
  return $ fromMaybe VUndef $ propValue <$> prop

-- ref 8.12.4, incomplete
objCanPut :: String -> Shared JSObj -> Runtime Bool
objCanPut p objRef = do
  return True

-- ref 8.12.5, incomplete
objPut :: String -> JSVal -> Bool -> Shared JSObj -> Runtime ()
objPut p v throw objRef = do
  canPut <- objCanPut p objRef
  unless canPut $ raiseError $ "TypeError: " ++ "Cannot set property " ++ p ++ " of object"
  ownDesc <- objGetOwnProperty p objRef
  case ownDesc of
    Just d -> objDefineOwnProperty p (propSetValue v d) throw objRef
    Nothing -> objDefineOwnProperty p (valueToProp v) throw objRef

-- ref 8.12.8, incomplete
objDefaultValue :: PrimitiveHint -> Shared JSObj -> Runtime JSVal
objDefaultValue hint objRef = case hint of
  HintString -> call "toString" `orElse` call "valueOf"
  HintNumber -> call "valueOf" `orElse` call "toString"
  HintNone   -> call "valueOf" `orElse` call "toString"
  where
    orElse :: Runtime (Maybe a) -> Runtime (Maybe a) -> Runtime a
    orElse p1 p2 = do
      m1 <- p1
      case m1 of
        Just r -> return r
        Nothing -> do
          m2 <- p2
          case m2 of
            Just r -> return r
            Nothing -> do
              o <- deref objRef
              raiseProtoError TypeError $ "Cannot get default value for " ++ show (ownProperties o)
    call :: String -> Runtime (Maybe JSVal)
    call method = do
      m <- objGet method objRef
      case m of
        VNative f -> do
          Just <$> f (VObj objRef) []
        VObj mRef -> do
          mm <- callMethod <$> deref mRef
          case mm of
            Nothing -> return Nothing
            Just f -> do
              result <- f (VObj objRef) []
              return $ if isPrimitive result
              then Just result
              else Nothing

        _ -> return Nothing

-- ref 8.12.9, incomplete
objDefineOwnProperty :: String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime ()
objDefineOwnProperty p desc throw objRef = do
  current <- objGetOwnProperty p objRef
  -- extensible <- objIsExtensible objRef
  let extensible = True
  case current of
    Nothing -> _objCreateOwnProperty p desc objRef
    Just desc' -> _objCreateOwnProperty p (propSetValue (propValue desc) desc') objRef


_objCreateOwnProperty :: String -> PropDesc JSVal -> Shared JSObj -> Runtime ()
_objCreateOwnProperty p desc = void . updateObj (objSetPropertyDescriptor p desc)


type ObjectModifier = Shared JSObj -> Runtime (Shared JSObj)

updateObj :: (JSObj -> JSObj) -> ObjectModifier
updateObj f objRef = modifyRef' objRef f

getGlobalProperty :: String -> Runtime JSVal
getGlobalProperty name = do
  liftM (maybe VUndef propValue) (objGetOwnProperty name =<< getGlobalObject)

objClassName :: Shared JSObj -> Runtime String
objClassName objRef = liftM objClass (deref objRef)


valClassName :: JSVal -> Runtime String
valClassName (VObj objRef) = objClassName objRef
valClassName _ = return "Object"

setClass :: String -> ObjectModifier
setClass cls = updateObj $ \obj -> obj { objClass = cls }

setCallMethod :: JSFunction -> ObjectModifier
setCallMethod f = updateObj $ \obj -> obj { callMethod = Just f }

setCstrMethod :: JSFunction -> ObjectModifier
setCstrMethod f = updateObj $ \obj -> obj { cstrMethod = Just f }

setPrimitiveValue :: JSVal -> ObjectModifier
setPrimitiveValue = updateObj . objSetProperty "valueOf" . wrapValInNativeFunc
  where wrapValInNativeFunc :: JSVal -> JSVal
        wrapValInNativeFunc v = VNative $ \_this _args -> return v

objSetPrototype :: Maybe (Shared JSObj) -> ObjectModifier
objSetPrototype prototype = updateObj $ \obj -> obj { objPrototype = prototype }


addOwnProperty :: String -> JSVal -> ObjectModifier
addOwnProperty name val = updateObj $ objSetProperty name val

objSetProperty :: String -> JSVal -> JSObj -> JSObj
objSetProperty name value obj = objSetPropertyDescriptor name (valueToProp value) obj

objSetPropertyDescriptor :: String -> PropDesc JSVal -> JSObj -> JSObj
objSetPropertyDescriptor name desc obj = obj { ownProperties = propMapInsert name desc (ownProperties obj) }

isWrapperFor :: (JSVal -> Runtime JSVal) -> JSVal -> String -> ObjectModifier
isWrapperFor f defaultValue name =
  updateObj $ \obj -> obj { objClass = name,
                            callMethod = Just call,
                            cstrMethod = Just cstr }
  where
    call _this args =
      if null args then return defaultValue else f (head args)
    cstr this args = do
      val <- call this args
      case this of
        VObj obj -> do
          VObj <$> (setClass name obj >>= setPrimitiveValue val)
        _ -> raiseError $ name ++ " constructor called with this = " ++ show this


fromHint :: PrimitiveHint -> JSVal
fromHint HintNone = VUndef
fromHint HintNumber = VNum 0
