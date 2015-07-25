{-# LANGUAGE FlexibleInstances #-}

module Polyvariadic where

import Data.Maybe
import Runtime
import qualified Data.Text as T
import Data.Text (Text)
import JSNum

class Polyvariadic a where
  papply :: a -> [JSVal] -> Runtime JSVal

class Convertible a where
  jsUnit :: a
  fromJS :: JSVal -> Runtime a
  toJS :: a -> JSVal

instance Convertible JSVal where
  jsUnit = VUndef
  fromJS = return
  toJS = id

instance Convertible Int where
  jsUnit = 0
  fromJS n = fromIntegral <$> toInt n
  toJS = VInt . fromIntegral

instance Convertible Double where
  jsUnit = 0
  fromJS = toNumber
  toJS = VNum

instance Convertible String where
  jsUnit = ""
  fromJS s = T.unpack <$> toString s
  toJS = VStr . T.pack

instance Convertible Text where
  jsUnit = T.empty
  fromJS = toString
  toJS = VStr

instance (Convertible b, Polyvariadic a) => Polyvariadic (b -> a) where
  papply f [] = papply (f jsUnit) []
  papply f (arg : args) = do { val <- fromJS arg ; papply (f val) args }

instance {-# OVERLAPS #-} (Convertible b, Polyvariadic a) => Polyvariadic (Maybe b -> a) where
  papply f [] = papply (f Nothing) []
  papply f (arg : args) = do { val <- fromJS arg ; papply (f $ Just val) args }

instance Convertible a => Polyvariadic (Maybe a) where
  papply Nothing _  = return VUndef
  papply (Just v) _ = return (toJS v)

instance Convertible a => Polyvariadic [a] where
  papply vs _ = createArray (map (Just . toJS) vs)

instance Convertible a => Polyvariadic (Runtime a) where
  papply v _ = toJS <$> v

instance Polyvariadic Double where
  papply d _ = return $ VNum d

instance Polyvariadic Int where
  papply n _ = return . VInt $ fromIntegral n

instance Polyvariadic Bool where
  papply b _ = return $ VBool b

instance Polyvariadic JSVal where
  papply v _ = return v

instance Polyvariadic String where
  papply s _ = return . VStr . T.pack $ s

instance Polyvariadic Text where
  papply ts _ = undefined

static :: Polyvariadic a => PropertyName -> Int -> a -> Builder ()
static name arity f = liftR (return $ papplyNoThis f) >>= method name arity

native :: Polyvariadic a => PropertyName -> Int -> a -> Builder ()
native name arity f = liftR (return $ papplyWithThis f) >>= method name arity

constant :: Polyvariadic a => PropertyName -> a -> Builder ()
constant name val = do
  v <- liftR $ papply val []
  descriptor name (dataPD v False False False)

papplyNoThis :: Polyvariadic a => a -> JSVal -> [JSVal] -> Runtime JSVal
papplyNoThis f _this args = papply f args

papplyWithThis :: Polyvariadic a => a -> JSVal -> [JSVal] -> Runtime JSVal
papplyWithThis f this args = papply f (this:args)
