{-# LANGUAGE RankNTypes #-}

module Runtime.Shared (share, shareLexEnv, sharePropertyMap, deref, modifyRef, modifyRef') where

import Control.Lens
import Data.Map.Lens
import Control.Monad.State
import qualified Data.Map as M
import Runtime.Global
import Runtime.Types


share :: JSObj -> Runtime (Shared JSObj)
share = shareShared globalObjStore

shareLexEnv :: LexEnv -> Runtime (Shared LexEnv)
shareLexEnv = shareShared globalLexEnvStore

sharePropertyMap :: PropertyMap -> Runtime (Shared PropertyMap)
sharePropertyMap = shareShared globalPropMapStore

deref :: Shared a -> Runtime a
deref (Shared l objId) = do
  g <- get
  maybe (error $ "Not found " ++ show objId) return $ view l g

modifyRef :: Shared a -> (a -> a) -> Runtime ()
modifyRef (Shared l objId) f = do
  g <- get
  put $ over l (fmap f) g

modifyRef' :: Shared a -> (a -> a) -> Runtime (Shared a)
modifyRef' shared f = modifyRef shared f >> return shared


shareShared :: Show a => Simple Lens JSGlobal (M.Map ObjId a) -> a -> Runtime (Shared a)
shareShared store obj = do
  objId <- nextID
  g <- get
  let m = view store g
      newmap = M.insert objId obj m
  put $ set store newmap g
  return $ Shared (store.at objId) objId

nextID :: Runtime ObjId
nextID = do
  global <- get
  let objId = 1 + globalNextID global
  put global { globalNextID = objId }
  return objId
