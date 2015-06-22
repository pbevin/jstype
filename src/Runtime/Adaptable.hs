{-# LANGUAGE FlexibleInstances #-}

module Runtime.Adaptable where

import Runtime.Types
import Runtime.Conversion
import Runtime.PropertyDescriptor
import JSNum

class Adaptable a where
  adapt :: a -> JSVal -> [JSVal] -> Runtime JSVal

instance Adaptable a => Adaptable ((->) Double a) where
  adapt f this []         = adapt (f 0) this []
  adapt f this (d : args) = do a <- toNumber d
                               adapt (f a) this args

instance Adaptable a => Adaptable (String -> a) where
  adapt f this []         = adapt (f "") this []
  adapt f this (d : args) = do a <- toString d
                               adapt (f a) this args

instance Adaptable Double where
  adapt d _ _ = return $ VNum d

instance Adaptable Int where
  adapt d _ _ = return . VNum $ fromIntegral d

