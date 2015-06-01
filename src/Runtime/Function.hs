module Runtime.Function (functionObject) where

import Safe

import Runtime.Types
import Runtime.Object
import Runtime.Conversion



functionObject :: Shared JSObj -> Runtime (Shared JSObj)
functionObject prototype = newObject >>= setClass "Function"
                                     >>= addOwnProperty "prototype" (VObj prototype)
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
      then raiseError "TypeError"
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
