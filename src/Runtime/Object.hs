{-# LANGUAGE LambdaCase #-}

module Runtime.Object where

import Data.Functor
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Runtime.Types
import Runtime.Global
import Runtime.PropMap
import Runtime.PropertyDescriptor
import Expr

import Debug.Trace

newObject :: Runtime (Shared JSObj)
newObject = do
  prototype <- globalObjectPrototype <$> get
  share $ emptyObject { objPrototype = prototype,
                        defineOwnPropertyMethod = Just objDefineOwnPropertyObject,
                        getMethod = Just objGetObj }

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


-- ref 8.12.3
objGetObj :: String -> Shared JSObj -> Runtime JSVal
objGetObj name objRef = do
  prop <- objGetProperty name objRef
  case prop of
    Nothing -> return VUndef
    Just desc -> if isDataDescriptor prop
                 then return $ fromMaybe VUndef (propValue desc)
                 else case propGetter desc of
                   Nothing -> return VUndef
                   Just f  -> f (VObj objRef)


-- ref 8.12.4
objCanPut :: String -> Shared JSObj -> Runtime Bool
objCanPut p o = do
  desc <- objGetOwnProperty p o
  case desc of
    Just d ->
      if isAccessorDescriptor desc
      then return $ isJust (propSetter d)
      else return $ propIsWritable d
    Nothing ->
      objPrototype <$> deref o >>= \case
        Nothing -> objExtensible <$> deref o
        Just _ -> do
          isExtensible <- objExtensible <$> deref o
          inherited <- objGetProperty p o
          case inherited of
            Nothing -> return isExtensible
            Just d' ->
              if isAccessorDescriptor inherited
              then return $ isJust (propSetter d')
              else return $ isExtensible && propIsWritable d'

-- ref 8.12.5
objPut :: String -> JSVal -> Bool -> Shared JSObj -> Runtime ()
objPut p v throw objRef = do
  canPut <- objCanPut p objRef
  if not canPut
  then if throw
       then raiseProtoError TypeError $ "Attempt to overwrite read-only property " ++ p
       else return ()
  else do
    ownDesc <- objGetOwnProperty p objRef
    if isDataDescriptor ownDesc
    then void $ defineOwnProperty p (dataPD v True True True) throw objRef
    else do
      desc <- objGetProperty p objRef
      if isAccessorDescriptor desc
      then case propSetter (fromJust desc) of
        Nothing -> return ()
        Just s -> s v
      else void $ defineOwnProperty p (dataPD v True True True) throw objRef

-- ref 8.12.6
objHasProperty :: String -> Shared JSObj -> Runtime Bool
objHasProperty p objRef = isJust <$> objGetProperty p objRef

-- ref 8.12.7
objDelete :: String -> Bool -> Shared JSObj -> Runtime JSVal
objDelete p throw objRef = do
  desc <- objGetOwnProperty p objRef
  case desc of
    Nothing -> return (VBool True)
    Just d -> if propIsConfigurable d
              then updateObj (objDeleteProperty p) objRef >> return (VBool True)
              else if throw
                   then raiseProtoError TypeError $ "Cannot remove non-existent property " ++ p
                   else return (VBool False)




-- ref 8.12.8, incomplete
objDefaultValue :: PrimitiveHint -> Shared JSObj -> Runtime JSVal
objDefaultValue hint objRef = case hint of
  HintString -> call "toString" `orElse` call "valueOf"
  HintNumber -> call "valueOf" `orElse` call "toString"
  HintNone   -> do
    cls <- objClass <$> deref objRef
    if cls == "Date"
    then objDefaultValue HintString objRef
    else objDefaultValue HintNumber objRef

  where
    orElse :: Show a => Runtime (Maybe a) -> Runtime (Maybe a) -> Runtime a
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
              raiseProtoError TypeError $ "Cannot get default value for object of type " ++ (objClass o) ++ " with properties " ++ show (ownProperties o)
    call :: String -> Runtime (Maybe JSVal)
    call method = do
      m <- objGet method objRef
      returnIfPrimitive <$> case m of
        VNative f -> do
          Just <$> f (VObj objRef) []
        VObj mRef -> do
          mm <- callMethod <$> deref mRef
          case mm of
            Nothing -> return Nothing
            Just f -> do
              Just <$> f (VObj objRef) []
        _ -> return Nothing


    returnIfPrimitive :: Maybe JSVal -> Maybe JSVal
    returnIfPrimitive Nothing = Nothing
    returnIfPrimitive (Just val) =
      if isPrimitive val
      then Just val
      else Nothing

-- ref 8.12.9, incomplete
objDefineOwnPropertyObject :: String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime (Shared JSObj)
objDefineOwnPropertyObject p desc throw objRef = do
  extensible <- objIsExtensible objRef
  objGetOwnProperty p objRef >>= \case
    Nothing ->
      if extensible
      then _objCreateOwnProperty p desc objRef
      else raiseProtoError TypeError $ "Can't add to non-extensible object"
    Just current -> _objUpdateOwnProperty p desc current throw objRef

_objCreateOwnProperty :: String -> PropDesc JSVal -> Shared JSObj -> Runtime (Shared JSObj)
_objCreateOwnProperty p desc = updateObj (objSetPropertyDescriptor p desc)

_objUpdateOwnProperty :: String -> PropDesc JSVal -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime (Shared JSObj)
_objUpdateOwnProperty p newDesc oldDesc throw o =
  let dOld = isDataDescriptor (Just oldDesc)
      dNew = isDataDescriptor (Just newDesc)
  in case (dOld, dNew) of
    (True, False) -> do
      rejectIf (not $ propIsConfigurable oldDesc)
      let g = propGetter newDesc
          s = propSetter newDesc
          e = propIsEnumerable oldDesc
          c = propIsConfigurable oldDesc
      updateObj (objSetPropertyDescriptor p $ accessorPD g s e c) o

    (False, True) -> do
      rejectIf (not $ propIsConfigurable oldDesc)
      let v = propValue newDesc
          w = propIsWritable newDesc
          e = propIsEnumerable oldDesc
          c = propIsConfigurable oldDesc
      updateObj (objSetPropertyDescriptor p $ dataPD' v w e c) o

    (True, True) -> do
      when (propIsConfigurable oldDesc == False) $
        when (propIsWritable oldDesc == False) $ do
          rejectIf (propIsWritable newDesc == True)
          rejectIf (sameValue (fromMaybe VUndef $ propValue oldDesc) (fromMaybe VUndef $ propValue newDesc))
      updateObj (objSetPropertyDescriptor p newDesc) o

    (False, False) -> do
      let g = propGetter newDesc <|> propGetter oldDesc
          s = propSetter newDesc <|> propSetter oldDesc
          e = propIsConfigurable oldDesc
          c = propIsEnumerable oldDesc
      updateObj (objSetPropertyDescriptor p (accessorPD g s e c)) o

  where rejectIf :: Bool -> Runtime ()
        rejectIf condition = when (condition && throw) $ raiseProtoError TypeError "Cannot overwrite property"




type ObjectModifier = Shared JSObj -> Runtime (Shared JSObj)

updateObj :: (JSObj -> JSObj) -> ObjectModifier
updateObj f objRef = modifyRef' objRef f

getGlobalProperty :: String -> Runtime JSVal
getGlobalProperty name = do
  getGlobalObject >>= objGet name

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

setGetMethod :: (String -> Shared JSObj -> Runtime JSVal) -> ObjectModifier
setGetMethod f = updateObj $ \obj -> obj { getMethod = Just f }

setScope :: Shared LexEnv -> ObjectModifier
setScope scope = updateObj $ \obj -> obj { objScope = Just scope }

setFormalParameters :: [Ident] -> ObjectModifier
setFormalParameters params = updateObj $ \obj -> obj { objFormalParameters = Just params }

setCode :: Program -> ObjectModifier
setCode prog = updateObj $ \obj -> obj { objCode = Just prog }

setPrimitiveValue :: JSVal -> ObjectModifier
setPrimitiveValue = updateObj . objSetProperty "valueOf" . wrapValInNativeFunc
  where wrapValInNativeFunc :: JSVal -> JSVal
        wrapValInNativeFunc v = VNative $ \_this _args -> return v

setPrimitiveToString :: JSVal -> ObjectModifier
setPrimitiveToString val = updateObj $ \obj -> objSetProperty "toString" (VNative f) obj
  where f _this _args = return val

objSetPrototype :: Shared JSObj -> ObjectModifier
objSetPrototype prototype = updateObj $ \obj -> obj { objPrototype = Just prototype }


addOwnProperty :: String -> JSVal -> ObjectModifier
addOwnProperty name val = updateObj $ objSetProperty name val

addOwnPropertyDescriptor :: String -> PropDesc JSVal -> ObjectModifier
addOwnPropertyDescriptor name val = updateObj $ objSetPropertyDescriptor name val

addOwnConstant :: String -> JSVal -> ObjectModifier
addOwnConstant name val = addOwnPropertyDescriptor name (dataPD val False False False)

objSetProperty :: String -> JSVal -> JSObj -> JSObj
objSetProperty name value obj = objSetPropertyDescriptor name (dataPD value True True True) obj

objSetPropertyDescriptor :: String -> PropDesc JSVal -> JSObj -> JSObj
objSetPropertyDescriptor name desc obj = obj { ownProperties = propMapInsert name desc (ownProperties obj) }

objDeleteProperty :: String -> JSObj -> JSObj
objDeleteProperty name obj = obj { ownProperties = propMapDelete name (ownProperties obj) }

objSetExtensible :: Bool -> ObjectModifier
objSetExtensible extensible = updateObj $ \obj -> obj { objExtensible = extensible }

objIsExtensible :: Shared JSObj -> Runtime Bool
objIsExtensible objRef = objExtensible <$> deref objRef

objSetPrimitive :: JSVal -> ObjectModifier
objSetPrimitive val = updateObj $ \obj -> obj { objPrimitiveValue = Just val }

objGetPrimitive :: Shared JSObj -> Runtime JSVal
objGetPrimitive objRef = fromMaybe VUndef . objPrimitiveValue <$> deref objRef

objSetParameterMap :: JSVal -> ObjectModifier
objSetParameterMap m = updateObj $ \obj -> obj { objParameterMap = Just m }

objGetParameterMap :: Shared JSObj -> Runtime JSVal
objGetParameterMap objRef = fromMaybe VUndef . objParameterMap <$> deref objRef

objSetHasInstance :: (Shared JSObj -> JSVal -> Runtime Bool) -> ObjectModifier
objSetHasInstance method = updateObj $ \obj -> obj { hasInstanceMethod = Just method }

objFindPrototype :: String -> Runtime (Shared JSObj)
objFindPrototype name =
  getGlobalProperty name >>= valGetPrototype >>= maybe oops return
    where
      valGetPrototype (VObj objRef) = fromObj <$> objGet "prototype" objRef
      valGetPrototype _ = return Nothing
      oops = raiseError $ "No prototype for " ++ name

-- propValue :: PropDesc a -> JSVal -> Runtime a
-- propValue pd this
--   | isDataDescriptor pd     = case getValue pd of
--                                 Nothing -> return VUndef
--                                 Just v  -> return v
--   | isAccessorDescriptor pd = case propGetter pd of
--                                 Nothing -> return VUndef
--                                 Just f -> f this
--   | otherwise               = return VUndef
