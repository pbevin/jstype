module Runtime.Object where

import Data.Functor
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Runtime.Types
import Debug.Trace

objSetProperty :: String -> JSVal -> JSObj -> JSObj
objSetProperty name value obj = obj { ownProperties = M.insert name value (ownProperties obj) }

-- ref 8.12.1, incomplete
objGetOwnProperty :: JSObj -> String -> Maybe JSVal
objGetOwnProperty obj name = M.lookup name (ownProperties obj)

-- ref 8.12.2, incomplete
objGetProperty :: String -> JSObj -> JSRuntime (Maybe JSVal)
objGetProperty name obj = maybe checkPrototype (return . Just) $ objGetOwnProperty obj name
  where checkPrototype = case objGetOwnProperty obj "prototype" of
          Just (VObj prototype) -> objGetProperty name =<< deref prototype
          _ -> return Nothing

-- ref 8.12.8, incomplete
objDefaultValue :: PrimitiveHint -> JSObj -> JSRuntime JSVal
objDefaultValue hint obj = return $ fromMaybe (fromHint hint) $ primitive obj

type ObjectModifier = Shared JSObj -> JSRuntime (Shared JSObj)

updateObj :: (JSObj -> JSObj) -> ObjectModifier
updateObj f objRef = modifyRef' objRef f

setClass :: String -> ObjectModifier
setClass cls = updateObj $ \obj -> obj { objClass = cls }

setCallMethod :: JSFunction -> ObjectModifier
setCallMethod f = updateObj $ \obj -> obj { callMethod = Just f }

setPrimitiveValue :: JSVal -> ObjectModifier
setPrimitiveValue v = updateObj $ \obj -> obj { primitive = Just v }

addOwnProperty :: String -> JSVal -> ObjectModifier
addOwnProperty name val = updateObj $ objSetProperty name val


fromHint :: PrimitiveHint -> JSVal
fromHint HintNone = VUndef
fromHint HintNumber = VNum 0
