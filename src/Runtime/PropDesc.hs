module Runtime.PropDesc where

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
