{-# LANGUAGE RankNTypes #-}

module Runtime.Shared (share, shareLexEnv, sharePropertyMap, deref, modifyRef, modifyRef') where

import Control.Lens
import Data.Map.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.IORef
import Runtime.Types


share :: JSObj -> Runtime (Shared JSObj)
share = shareShared

shareLexEnv :: LexEnv -> Runtime (Shared LexEnv)
shareLexEnv = shareShared

sharePropertyMap :: PropertyMap -> Runtime (Shared PropertyMap)
sharePropertyMap = shareShared

deref :: Shared a -> Runtime a
deref (Shared ref objId) =
  liftIO $ readIORef ref

modifyRef :: Shared a -> (a -> a) -> Runtime ()
modifyRef (Shared ref _) f = liftIO $ modifyIORef ref f

modifyRef' :: Shared a -> (a -> a) -> Runtime (Shared a)
modifyRef' shared f = modifyRef shared f >> return shared


shareShared :: Show a => a -> Runtime (Shared a)
shareShared obj = do
  objId <- nextID
  ref <- liftIO $ newIORef obj
  return $ Shared ref objId
  -- g <- get
  -- let m = view store g
  --     newmap = M.insert objId obj m
  -- put $ set store newmap g
  -- return $ Shared (store.at objId) objId

nextID :: Runtime ObjId
nextID = do
  store <- get
  let objId = 1 + storeNextID store
  put store { storeNextID = objId }
  return objId
