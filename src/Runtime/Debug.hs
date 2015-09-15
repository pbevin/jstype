{-# LANGUAGE OverloadedStrings #-}
module Runtime.Debug (debugVal) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Control.Lens
import Runtime.Object
import Runtime.Conversion
import Runtime.Types
import Runtime.PropMap
import Runtime.PropertyDescriptor
import Runtime.Shared
import Expr

debugVal :: JSVal -> Runtime JSVal
debugVal val = do
  prettify val >>= debug
  return val

prettify :: JSVal -> Runtime Text
prettify val =
  case val of
    VObj obj -> do
      props <- propMapToList . view ownProperties <$> deref obj
      return $ "{ " <> T.intercalate ", " (map prettyProp props) <> " }"
    _ ->
      toString val

prettyProp :: (Ident, PropDesc JSVal) -> Text
prettyProp (key, val) = key <> ": " <> (maybe "" showVal (propValue val))
