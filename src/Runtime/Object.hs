{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Runtime.Object where

import Control.Lens
import Data.Functor
import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Runtime.Types
import Runtime.Global
import Runtime.Shared
import Runtime.PropMap
import Runtime.PropertyDescriptor
import Expr

import Debug.Trace

newObject :: Runtime (Shared JSObj)
newObject = do
  prototype <- asks globalObjectPrototype
  share $ set objPrototype (Just prototype) defaultObject

defaultObject :: JSObj
defaultObject = emptyObject { _defineOwnPropertyMethod = Just objDefineOwnPropertyObject,
                              _getOwnPropertyMethod = Just objGetOwnPropertyObj,
                              _deleteMethod = Just objDeleteObject,
                              _getMethod = Just objGetObj }

-- ref 8.12.1
objGetOwnPropertyObj :: Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
objGetOwnPropertyObj name objRef = liftM (propMapLookup name . view ownProperties) (deref objRef)

-- ref 8.12.2
objGetProperty :: Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
objGetProperty name objRef = do
  prop <- objGetOwnProperty name objRef
  case prop of
    Just _ -> return prop
    Nothing -> do
      proto <- view objPrototype <$> deref objRef
      case proto of
        Just p -> objGetProperty name p
        Nothing -> return Nothing

valGetProperty :: Text -> JSVal -> Runtime (Maybe (PropDesc JSVal))
valGetProperty name (VObj objRef) = objGetProperty name objRef
valGetProperty _ _ = return Nothing


-- ref 8.12.3
objGetObj :: Text -> Shared JSObj -> Runtime JSVal
objGetObj name objRef = fromMaybe VUndef <$> objGetMaybe name objRef

objGetMaybe :: Text -> Shared JSObj -> Runtime (Maybe JSVal)
objGetMaybe name objRef = do
  prop <- objGetProperty name objRef
  case prop of
    Nothing -> return Nothing
    Just desc -> if isDataDescriptor prop
                 then return $ propValue desc
                 else case propGetter desc of
                   Nothing -> return Nothing
                   Just f  -> Just <$> f (VObj objRef)


-- ref 8.12.4
objCanPut :: Text -> Shared JSObj -> Runtime Bool
objCanPut p o = do
  desc <- objGetOwnProperty p o
  case desc of
    Just d ->
      if isAccessorDescriptor desc
      then return $ isJust (propSetter d)
      else return $ propIsWritable d
    Nothing ->
      view objPrototype <$> deref o >>= \case
        Nothing -> view objExtensible <$> deref o
        Just _ -> do
          isExtensible <- view objExtensible <$> deref o
          inherited <- objGetProperty p o
          case inherited of
            Nothing -> return isExtensible
            Just d' ->
              if isAccessorDescriptor inherited
              then return $ isJust (propSetter d')
              else return $ isExtensible && propIsWritable d'

-- ref 8.12.5
objPut :: Text -> JSVal -> Bool -> Shared JSObj -> Runtime ()
objPut p v throw objRef = do
  canPut <- objCanPut p objRef
  if not canPut
  then when throw $ raiseProtoError TypeError . T.unpack $ "Attempt to overwrite read-only property " <> p
  else do
    ownDesc <- objGetOwnProperty p objRef
    if isDataDescriptor ownDesc
    then void $ defineOwnProperty p (valuePD v) throw objRef
    else do
      desc <- objGetProperty p objRef
      if isAccessorDescriptor desc
      then case propSetter (fromJust desc) of
        Nothing -> return ()
        Just s -> s (VObj objRef) v
      else void $ defineOwnProperty p (dataPD v True True True) throw objRef

-- ref 8.12.6
objHasProperty :: Text -> Shared JSObj -> Runtime Bool
objHasProperty p objRef = isJust <$> objGetProperty p objRef

-- ref 8.12.7
objDeleteObject :: Text -> Bool -> Shared JSObj -> Runtime JSVal
objDeleteObject p throw objRef = do
  desc <- objGetOwnProperty p objRef
  case desc of
    Nothing -> return (VBool True)
    Just d -> if propIsConfigurable d
              then updateObj (objDeleteProperty p) objRef >> return (VBool True)
              else if throw
                   then raiseProtoError TypeError . T.unpack $ "Cannot remove non-existent property " <> p
                   else return (VBool False)

objDelete :: Text -> Bool -> Shared JSObj -> Runtime JSVal
objDelete p throw objRef = do
  view deleteMethod <$> deref objRef >>= \case
    Nothing -> raiseError $ "No delete method for " <> T.pack (show objRef)
    Just f -> f p throw objRef



-- ref 8.12.8, incomplete
objDefaultValue :: PrimitiveHint -> Shared JSObj -> Runtime JSVal
objDefaultValue hint objRef = case hint of
  HintString -> call "toString" `orElse` call "valueOf"
  HintNumber -> call "valueOf" `orElse` call "toString"
  HintNone   -> do
    cls <- view objClass <$> deref objRef
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
              raiseProtoError TypeError . T.unpack $
                T.unwords
                  [ "Cannot get default value for object of type",
                   (o^.objClass),
                   "with properties",
                   T.pack . show $ o^.ownProperties ]
    call :: Text -> Runtime (Maybe JSVal)
    call method = do
      m <- objGet method objRef
      returnIfPrimitive <$> case m of
        VNative _ _ f -> do
          Just <$> f (VObj objRef) []
        VObj mRef -> do
          mm <- view callMethod <$> deref mRef
          case mm of
            Nothing -> return Nothing
            Just f  -> Just <$> f (VObj objRef) []
        _ -> return Nothing


    returnIfPrimitive :: Maybe JSVal -> Maybe JSVal
    returnIfPrimitive Nothing = Nothing
    returnIfPrimitive (Just val) =
      if isPrimitive val
      then Just val
      else Nothing

-- ref 8.12.9, incomplete
objDefineOwnPropertyObject :: Text -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool
objDefineOwnPropertyObject p desc throw objRef = do
  extensible <- objIsExtensible objRef
  objGetOwnProperty p objRef >>= \case
    Nothing ->
      if extensible
      then _objCreateOwnProperty p desc objRef >> return True
      else raiseProtoError TypeError $ "Can't add to non-extensible object"
    Just current -> _objUpdateOwnProperty p desc current throw objRef >> return True

_objCreateOwnProperty :: Text -> PropDesc JSVal -> Shared JSObj -> Runtime (Shared JSObj)
_objCreateOwnProperty p desc = updateObj (objSetPropertyDescriptor p desc)

_objUpdateOwnProperty :: Text -> PropDesc JSVal -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime (Shared JSObj)
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
      when (not $ propIsConfigurable oldDesc) $
        when (not $ propIsWritable oldDesc) $ do
          rejectIf (propIsWritable newDesc)
          rejectIf (sameValue (fromMaybe VUndef $ propValue oldDesc) (fromMaybe VUndef $ propValue newDesc))
      case propValue newDesc of
        Nothing -> return o
        Just v  -> updateObj (objSetPropertyDescriptor p $ oldDesc `mappend` valuePD v) o

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

getGlobalProperty :: Text -> Runtime JSVal
getGlobalProperty name = do
  getGlobalObject >>= objGet name

objClassName :: Shared JSObj -> Runtime Text
objClassName objRef = view objClass <$> (deref objRef)


valClassName :: JSVal -> Runtime Text
valClassName (VObj objRef) = objClassName objRef
valClassName _ = return "Object"

setClass :: Text -> ObjectModifier
setClass cls = updateObj $ set objClass cls

setCallMethod :: JSFunction -> ObjectModifier
setCallMethod f = updateObj $ set callMethod (Just f)

setCstrMethod :: JSFunction -> ObjectModifier
setCstrMethod f = updateObj $ set cstrMethod (Just f)

setGetMethod :: (Text -> Shared JSObj -> Runtime JSVal) -> ObjectModifier
setGetMethod f = updateObj $ set getMethod (Just f)

setDeleteMethod :: (Text -> Bool -> Shared JSObj -> Runtime JSVal) -> ObjectModifier
setDeleteMethod f = updateObj $ set deleteMethod (Just f)

setGetOwnPropertyMethod :: (Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))) -> ObjectModifier
setGetOwnPropertyMethod f = updateObj $ set getOwnPropertyMethod (Just f)

setDefineOwnPropertyMethod :: (Text -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool) -> ObjectModifier
setDefineOwnPropertyMethod f = updateObj $ set defineOwnPropertyMethod (Just f)

setHostData :: HostData -> ObjectModifier
setHostData d = updateObj $ set objHostData d

setScope :: Shared LexEnv -> ObjectModifier
setScope scope = updateObj $ set objScope (Just scope)

setFormalParameters :: [Ident] -> ObjectModifier
setFormalParameters params = updateObj $ set objFormalParameters (Just params)

setCode :: Program -> ObjectModifier
setCode prog = updateObj $ set objCode (Just prog)

objSetPrototype :: Shared JSObj -> ObjectModifier
objSetPrototype prototype = updateObj $ set objPrototype (Just prototype)


addOwnProperty :: Text -> JSVal -> ObjectModifier
addOwnProperty name val = updateObj $ objSetProperty name val

addMethod :: Text -> Int -> JSFunction -> ObjectModifier
addMethod name len f = addOwnProperty name (VNative name len f)

addOwnPropertyDescriptor :: Text -> PropDesc JSVal -> ObjectModifier
addOwnPropertyDescriptor name val = updateObj $ objSetPropertyDescriptor name val

addOwnPropDesc :: Text -> PropDesc JSVal -> ObjectModifier
addOwnPropDesc = addOwnPropertyDescriptor

addOwnConstant :: Text -> JSVal -> ObjectModifier
addOwnConstant name val = addOwnPropertyDescriptor name (dataPD val False False False)

objSetProperty :: Text -> JSVal -> JSObj -> JSObj
objSetProperty name value obj = objSetPropertyDescriptor name (dataPD value True False True) obj

objSetPropertyDescriptor :: Text -> PropDesc JSVal -> JSObj -> JSObj
objSetPropertyDescriptor name desc = over ownProperties (propMapInsert name desc)

objDeleteProperty :: Text -> JSObj -> JSObj
objDeleteProperty name = over ownProperties (propMapDelete name)

objSetExtensible :: Bool -> ObjectModifier
objSetExtensible extensible = updateObj $ set objExtensible extensible

objIsExtensible :: Shared JSObj -> Runtime Bool
objIsExtensible objRef = view objExtensible <$> deref objRef

objSetPrimitive :: JSVal -> ObjectModifier
objSetPrimitive val = updateObj $ set objPrimitiveValue (Just val)

objGetPrimitive :: Shared JSObj -> Runtime JSVal
objGetPrimitive objRef = fromMaybe VUndef . view objPrimitiveValue <$> deref objRef

objSetParameterMap :: Shared JSObj -> ObjectModifier
objSetParameterMap m = updateObj $ set objParameterMap (Just m)

objSetHasInstance :: (Shared JSObj -> JSVal -> Runtime Bool) -> ObjectModifier
objSetHasInstance method = updateObj $ set hasInstanceMethod (Just method)

objFindPrototype :: Text -> Runtime (Shared JSObj)
objFindPrototype name =
  getGlobalProperty name >>= valGetPrototype >>= maybe oops return
    where
      valGetPrototype (VObj objRef) = fromObj <$> objGet "prototype" objRef
      valGetPrototype _ = return Nothing
      oops = raiseError $ "No prototype for " <> name

-- propValue :: PropDesc a -> JSVal -> Runtime a
-- propValue pd this
--   | isDataDescriptor pd     = case getValue pd of
--                                 Nothing -> return VUndef
--                                 Just v  -> return v
--   | isAccessorDescriptor pd = case propGetter pd of
--                                 Nothing -> return VUndef
--                                 Just f -> f this
--   | otherwise               = return VUndef

defineOwnProperty :: Text -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool
defineOwnProperty name desc strictness objRef = (view defineOwnPropertyMethod <$> deref objRef) >>= \case
  Nothing -> raiseProtoError ReferenceError "No defineOwnProperty method"
  Just m -> m name desc strictness objRef

objGet :: Text -> Shared JSObj -> Runtime JSVal
objGet p obj = do
  view getMethod <$> deref obj >>= \case
    Nothing -> raiseError $ "No get method for " <> T.pack (show obj)
    Just f -> f p obj

objGetOwnProperty :: Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
objGetOwnProperty p obj = do
  view getOwnPropertyMethod <$> deref obj >>= \case
    Nothing -> raiseError $ "No getOwnProperty method for " <> T.pack (show obj)
    Just f -> f p obj

isCallable :: JSVal -> Runtime (Maybe JSFunction)
isCallable (VObj obj) = view callMethod <$> deref obj
isCallable _ = return Nothing


fromObject :: a -> (JSObj -> a) -> JSVal -> Runtime a
fromObject _ f (VObj obj) = f <$> deref obj
fromObject a _ _          = return a

assertType :: JSType -> JSVal -> Runtime ()
assertType TypeBoolean (VBool _) = return ()
assertType TypeString  (VStr _)  = return ()
assertType TypeNumber  (VNum _)  = return ()
assertType TypeNumber  (VInt _)  = return ()
assertType ty (VObj obj) = do
  cls <- view objClass <$> deref obj
  case (ty, cls) of
    (TypeBoolean, "Boolean") -> return ()
    (TypeString, "String") -> return ()
    (TypeNumber, "Number") -> return ()
    _ -> raiseProtoError TypeError "Wrong type"
assertType _ _ = raiseProtoError TypeError "Wrong type!"
