module Runtime.Object where

import Data.Functor
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Runtime.Types
import Runtime.Global

import Debug.Trace

newObject :: Runtime (Shared JSObj)
newObject = do
  prototype <- getGlobalObjectPrototype
  share JSObj { objClass = "Object",
                ownProperties = M.fromList [("prototype", VObj prototype)],
                callMethod = Nothing,
                cstrMethod = Nothing,
                primitive = Nothing }

objSetProperty :: String -> JSVal -> JSObj -> JSObj
objSetProperty name value obj = obj { ownProperties = M.insert name value (ownProperties obj) }

-- ref 8.12.1, incomplete
objGetOwnProperty :: JSObj -> String -> Maybe JSVal
objGetOwnProperty obj name = M.lookup name (ownProperties obj)

-- ref 8.12.2, incomplete
objGetProperty :: String -> JSObj -> Runtime (Maybe JSVal)
objGetProperty name obj = maybe checkPrototype (return . Just) $ objGetOwnProperty obj name
  where checkPrototype = case objGetOwnProperty obj "prototype" of
          Just (VObj prototype) -> objGetProperty name =<< deref prototype
          _ -> return Nothing

valGetProperty :: String -> JSVal -> Runtime (Maybe JSVal)
valGetProperty name (VObj objRef) = deref objRef >>= objGetProperty name
valGetProperty _ _ = return Nothing

-- ref 8.12.8, incomplete
objDefaultValue :: PrimitiveHint -> JSObj -> Runtime JSVal
objDefaultValue hint obj = return $ fromMaybe (fromHint hint) $ primitive obj

type ObjectModifier = Shared JSObj -> Runtime (Shared JSObj)

updateObj :: (JSObj -> JSObj) -> ObjectModifier
updateObj f objRef = modifyRef' objRef f

getGlobalProperty :: String -> Runtime JSVal
getGlobalProperty name = do
  obj <- getGlobalObject
  deref obj >>= return . fromMaybe (VUndef) . flip objGetOwnProperty name

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
setPrimitiveValue v = updateObj $ \obj -> obj { primitive = Just v }

addOwnProperty :: String -> JSVal -> ObjectModifier
addOwnProperty name val = updateObj $ objSetProperty name val

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
