{-# LANGUAGE OverloadedStrings #-}

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
                             >>= addOwnProperty "set" (setter s)
                             >>= addOwnProperty "enumerable" (VBool e)
                             >>= addOwnProperty "configurable" (VBool c)
            debug (d, g, s, e, c)
            return $ VObj obj
  | otherwise = return VUndef


getter :: Maybe (JSVal -> Runtime JSVal) -> JSVal
getter Nothing = VUndef
getter (Just f) = VNative "getter" 0 $ \this _args -> f this

setter :: Maybe (JSVal -> JSVal -> Runtime ()) -> JSVal
setter Nothing = VUndef
setter (Just f) = VNative "setter" 1 $ \this args -> f this (headDef VUndef args) >> return VUndef
