{-# LANGUAGE FlexibleInstances #-}

module Runtime.Adaptable where

import Data.Maybe
import Runtime.Types
import Runtime.Conversion
import Runtime.PropertyDescriptor
import JSNum

class Adaptable a where
  adapt :: a -> JSVal -> [JSVal] -> Runtime JSVal

instance Adaptable a => Adaptable (Int -> a) where
  adapt f this []         = adapt (f 0) this []
  adapt f this (d : args) = do a <- toInt d
                               adapt (f a) this args

instance Adaptable a => Adaptable (Double -> a) where
  adapt f this []         = adapt (f 0) this []
  adapt f this (d : args) = do a <- toNumber d
                               adapt (f a) this args

instance Adaptable a => Adaptable (String -> a) where
  adapt f this []         = adapt (f "") this []
  adapt f this (d : args) = do a <- toString d
                               adapt (f a) this args

instance (Adaptable a) => Adaptable (Maybe Int -> a) where
  adapt f this []         = adapt (f Nothing) this []
  adapt f this (d : args) = do a <- Just <$> toInt d
                               adapt (f a) this args

instance Adaptable Double where
  adapt d _ _ = return $ VNum d

instance Adaptable Int where
  adapt n _ _ = return . VNum $ fromIntegral n

instance Adaptable JSVal where
  adapt v _ _ = return v

instance Adaptable String where
  adapt s _ _ = return . VStr $ s

instance Adaptable (Maybe Double) where
  adapt d _ _ = return . VNum $ fromMaybe jsNaN d

instance Adaptable (Runtime String) where
  adapt v _ _ = VStr <$> v
