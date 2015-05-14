module Runtime.Object where

import Data.Functor
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as M
import Data.IORef
import Runtime.Types


newObject :: JSRuntime (IORef JSObj)
newObject = liftIO $ do
  prototype <- newIORef objectPrototype
  newIORef JSObj { objClass = "Object",
                   ownProperties = M.fromList [("prototype", VObj prototype)],
                   callMethod = uncallable }

objectPrototype :: JSObj
objectPrototype = JSObj { objClass = "Object",
                          ownProperties = M.fromList [("prototype", VUndef)],
                          callMethod = uncallable }

objSetProperty :: String -> JSVal -> JSObj -> JSObj
objSetProperty name value obj = obj { ownProperties = M.insert name value (ownProperties obj) }

-- objCreate :: M.Map String JSVal -> M.Map String (IORef JSVal) -> JSRuntime (IORef JSObj)
-- objCreate internals ownProperties = liftIO $ newIORef $ JSObj internals ownProperties


-- ref 8.12.1, incomplete
objGetOwnProperty :: JSObj -> String -> JSRuntime (Maybe JSVal)
objGetOwnProperty obj name = liftIO $ return $ M.lookup name (ownProperties obj)

-- ref 8.12.2, incomplete
objGetProperty :: JSObj -> String -> JSRuntime (Maybe JSVal)
-- objGetProperty objref key = liftIO $ readIORef objref >>= \obj -> return $ fromJust $ M.lookup key $ objInternal obj
objGetProperty obj name = do
  prop <- objGetOwnProperty obj name
  case prop of
    Just _ -> return prop
    Nothing -> do
      let prototype = M.lookup "prototype" (ownProperties obj)
      case prototype of
        Just (VObj obj) -> do
          proto <- liftIO $ readIORef obj
          objGetProperty proto name
        _ -> return Nothing

uncallable :: JSVal -> [JSVal] -> JSRuntime JSVal
uncallable _ _ = error "Can't call this object"
