module Runtime.PropDesc (fromPropertyDescriptor) where

import Safe
import Runtime.Types
import Runtime.Object



-- ref 8.10.4
fromPropertyDescriptor :: Maybe (PropDesc JSVal) -> Runtime JSVal
fromPropertyDescriptor Nothing = return VUndef
fromPropertyDescriptor (Just desc) = case desc of
  (DataPD v w e c) -> do
    obj <- newObject >>= addOwnProperty "value" v
                     >>= addOwnProperty "writable" (VBool w)
                     >>= addOwnProperty "enumerable" (VBool e)
                     >>= addOwnProperty "configurable" (VBool c)
    return $ VObj obj
  (AccessorPD g s e c) -> do
    obj <- newObject >>= addOwnProperty "get" (getter g)
                     >>= addOwnProperty "writable" (setter s)
                     >>= addOwnProperty "enumerable" (VBool e)
                     >>= addOwnProperty "configurable" (VBool c)
    return $ VObj obj


getter :: Maybe (JSVal -> Runtime JSVal) -> JSVal
getter Nothing = VUndef
getter (Just f) = VNative $ \this _args -> f this

setter :: Maybe (JSVal -> Runtime ()) -> JSVal
setter Nothing = VUndef
setter (Just f) = VNative $ \this args -> f (headDef VUndef args) >> return VUndef
