module Runtime.Shared (share, shareLexEnv, sharePropertyMap, deref, modifyRef, modifyRef') where

import Control.Monad.State
import qualified Data.Map as M
import Runtime.Global
import Runtime.Types


share :: JSObj -> Runtime (Shared JSObj)
share = shareShared globalObjStore (\g s -> g { globalObjStore = s })

shareLexEnv :: LexEnv -> Runtime (Shared LexEnv)
shareLexEnv = shareShared globalLexEnvStore (\g s -> g { globalLexEnvStore = s })

sharePropertyMap :: PropertyMap -> Runtime (Shared PropertyMap)
sharePropertyMap = shareShared globalPropMapStore (\g s -> g { globalPropMapStore = s })

deref :: Shared a -> Runtime a
deref a = _g a (objid a) >>= maybe (error $ "Not found " ++ show (objid a)) return
modifyRef :: Shared a -> (a -> a) -> Runtime ()
modifyRef a f = _m a f (objid a)
modifyRef' :: Shared a -> (a -> a) -> Runtime (Shared a)
modifyRef' a f = _m a f (objid a) >> return a


getShared :: (JSGlobal -> M.Map ObjId a) -> ObjId -> Runtime (Maybe a)
getShared store objid = do
  theStore <- store <$> get
  return $ M.lookup objid theStore

modifyShared :: (JSGlobal -> M.Map ObjId a) -> (JSGlobal -> M.Map ObjId a -> JSGlobal) -> (a -> a) -> ObjId -> Runtime ()
modifyShared store sstore f objid = do
  g <- get
  put $ sstore g (M.alter (fmap f) objid (store g))
  return ()

shareShared :: Show a => (JSGlobal -> M.Map ObjId a) -> (JSGlobal -> M.Map ObjId a -> JSGlobal) -> a -> Runtime (Shared a)
shareShared store sstore obj = do
  objId <- nextID
  g <- get
  put $ sstore g (M.insert objId obj (store g))
  return $ Shared (getShared store) (modifyShared store sstore) objId

nextID :: Runtime ObjId
nextID = do
  global <- get
  let objId = 1 + globalNextID global
  put global { globalNextID = objId }
  return objId
