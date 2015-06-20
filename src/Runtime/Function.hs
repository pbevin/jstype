module Runtime.Function where

import Safe

import Runtime.Types
import Runtime.Object
import Runtime.Error
import Runtime.Shared
import Runtime.Conversion



functionObject :: String -> Shared JSObj -> Runtime (Shared JSObj)
functionObject name prototype =
  newObject
    >>= setClass "Function"
    >>= addOwnProperty "prototype" (VObj prototype)
    >>= addOwnProperty "name" (VStr name)
    >>= objSetHasInstance funHasInstance


-- ref 15.3.5.3
funHasInstance :: Shared JSObj -> JSVal -> Runtime Bool
funHasInstance f val = case val of
  VObj obj -> hasInstance' obj
  _ -> return False
  where
    hasInstance' :: Shared JSObj -> Runtime Bool
    hasInstance' v = do
      o <- objGet "prototype" f
      if typeof o /= TypeObject
      then raiseTypeError "TypeError"
      else searchPrototypes o v

    searchPrototypes :: JSVal -> Shared JSObj -> Runtime Bool
    searchPrototypes o v = do
      v' <- objPrototype <$> deref v
      case v' of
        Nothing -> return False
        Just p  -> do
          if o == VObj p
          then return True
          else searchPrototypes o p
