module Runtime.Object where

import Data.Functor
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as M
import Data.IORef
import Runtime.Types


newObject :: JSRuntime (IORef JSObj)
newObject = liftIO $ newIORef blankObject


blankObject :: JSObj
blankObject = JSObj { objPrototype = Just objectPrototype,
                      objClass = "Object",
                      ownProperties = M.empty,
                      callMethod = uncallable }

objectPrototype :: JSObj
objectPrototype = JSObj { objPrototype = Nothing,
                          objClass = "Object",
                          ownProperties = M.empty,
                          callMethod = uncallable }

-- objCreate :: M.Map String JSVal -> M.Map String (IORef JSVal) -> JSRuntime (IORef JSObj)
-- objCreate internals ownProperties = liftIO $ newIORef $ JSObj internals ownProperties


-- ref 8.12.1, incomplete
objGetOwnProperty :: IORef JSObj -> String -> JSRuntime (Maybe JSVal)
objGetOwnProperty objref name = liftIO $ do
  obj <- readIORef objref
  let val = M.lookup name (ownProperties obj)
  case val of
    Nothing -> return Nothing
    Just ref -> Just <$> readIORef ref


-- ref 8.12.2, incomplete
objGetProperty :: IORef JSObj -> String -> JSRuntime (Maybe JSVal)
-- objGetProperty objref key = liftIO $ readIORef objref >>= \obj -> return $ fromJust $ M.lookup key $ objInternal obj
objGetProperty = objGetOwnProperty

uncallable :: JSObj -> JSVal -> [JSVal] -> JSRuntime JSVal
uncallable _ _ _ = error "Can't call this object"
