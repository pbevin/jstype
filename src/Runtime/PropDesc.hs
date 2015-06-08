module Runtime.PropDesc (fromPropertyDescriptor) where

import Safe
import Data.Maybe
import Runtime.PropertyDescriptor
import Runtime.Types
import Runtime.Object



-- ref 8.10.4
fromPropertyDescriptor :: Maybe (PropDesc JSVal) -> Runtime JSVal
fromPropertyDescriptor desc
  | isDataDescriptor desc =
      let d = fromJust desc
          v = propValue d
          w = propIsWritable d
          e = propIsEnumerable d
          c = propIsConfigurable d
      in do obj <- newObject >>= addOwnProperty "value" (fromMaybe VUndef v)
                             >>= addOwnProperty "writable" (VBool w)
                             >>= addOwnProperty "enumerable" (VBool e)
                             >>= addOwnProperty "configurable" (VBool c)
            return $ VObj obj
  | isAccessorDescriptor desc =
      let d = fromJust desc
          g = propGetter d
          s = propSetter d
          e = propIsEnumerable d
          c = propIsConfigurable d
      in do obj <- newObject >>= addOwnProperty "get" (getter g)
                             >>= addOwnProperty "writable" (setter s)
                             >>= addOwnProperty "enumerable" (VBool e)
                             >>= addOwnProperty "configurable" (VBool c)
            return $ VObj obj
  | otherwise = return VUndef


getter :: Maybe (JSVal -> Runtime JSVal) -> JSVal
getter Nothing = VUndef
getter (Just f) = VNative $ \this _args -> f this

setter :: Maybe (JSVal -> Runtime ()) -> JSVal
setter Nothing = VUndef
setter (Just f) = VNative $ \this args -> f (headDef VUndef args) >> return VUndef
