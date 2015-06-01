module Runtime.Function (functionObject, mkFunction) where

import Safe

import Runtime.Types
import Runtime.Object
import Runtime.Error
import Runtime.Conversion



functionObject :: String -> Shared JSObj -> Runtime (Shared JSObj)
functionObject name prototype = newObject >>= mkFunction name prototype

mkFunction :: String -> Shared JSObj -> Shared JSObj -> Runtime (Shared JSObj)
mkFunction name prototype this =
  setClass "Function" this
    >>= addOwnProperty "prototype" (VObj prototype)
    >>= addOwnProperty "name" (VStr name)
    >>= objSetHasInstance hasInstance


-- ref 15.3.5.3
hasInstance :: Shared JSObj -> JSVal -> Runtime Bool
hasInstance f val = case val of
  VObj obj -> hasInstance' f obj
  _ -> return False
  where
    hasInstance' :: Shared JSObj -> Shared JSObj -> Runtime Bool
    hasInstance' f val = do
      o <- objGet "prototype" f
      if typeof o /= TypeObject
      then raiseTypeError "TypeError"
      else searchPrototypes o val

    searchPrototypes :: JSVal -> Shared JSObj -> Runtime Bool
    searchPrototypes o v = do
      v' <- objPrototype <$> deref v
      case v' of
        Nothing -> return False
        Just p  -> do
          if o == VObj p
          then return True
          else searchPrototypes o p
