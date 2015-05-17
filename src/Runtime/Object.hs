module Runtime.Object where

import Data.Functor
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as M
import Runtime.Types

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
objDefaultValue _ _ = return VUndef
