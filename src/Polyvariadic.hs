{-# LANGUAGE FlexibleInstances #-}

module Polyvariadic where

import Data.Maybe
import Runtime
import qualified Data.Text as T
import Data.Text (Text)
import JSNum

class Polyvariadic a where
  papply :: a -> [JSVal] -> Runtime JSVal

instance Polyvariadic a => Polyvariadic (Int -> a) where
  papply f []         = papply (f 0) []
  papply f (d : args) = do a <- toInt d
                           papply (f a) args

instance Polyvariadic a => Polyvariadic (Double -> a) where
  papply f []         = papply (f 0) []
  papply f (d : args) = do a <- toNumber d
                           papply (f a) args

instance Polyvariadic a => Polyvariadic (String -> a) where
  papply f []         = papply (f "") []
  papply f (d : args) = do a <- toString d
                           papply (f a) args

instance Polyvariadic a => Polyvariadic (Text -> a) where
  papply f []         = papply (f T.empty) []
  papply f (d : args) = do a <- toString d
                           papply (f (T.pack a)) args

instance Polyvariadic a => Polyvariadic (Maybe Text -> a) where
  papply f []         = papply (f Nothing) []
  papply f (d : args) = do a <- toString d
                           papply (f (Just $ T.pack a)) args

instance (Polyvariadic a) => Polyvariadic (Maybe Int -> a) where
  papply f []         = papply (f Nothing) []
  papply f (d : args) = do a <- Just <$> toInt d
                           papply (f a) args

instance (Polyvariadic a) => Polyvariadic (Maybe String -> a) where
  papply f []         = papply (f Nothing) []
  papply f (d : args) = do a <- Just <$> toString d
                           papply (f a) args

instance Polyvariadic Double where
  papply d _ = return $ VNum d

instance Polyvariadic Int where
  papply n _ = return . VInt $ fromIntegral n

instance Polyvariadic JSVal where
  papply v _ = return v

instance Polyvariadic String where
  papply s _ = return . VStr $ s

instance Polyvariadic (Maybe Double) where
  papply d _ = return . VNum $ fromMaybe jsNaN d

instance Polyvariadic (Runtime String) where
  papply v _ = VStr <$> v

instance Polyvariadic [Text] where
  papply ts _ = createArray . map (Just . VStr . T.unpack) $ ts

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
