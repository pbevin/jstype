{-# LANGUAGE LambdaCase #-}

module Runtime.Arguments (createArgumentsObject) where

import Prelude hiding (map)
import Control.Lens hiding (strict)
import Control.Monad (forM_, when, void)
import Data.Maybe
import Runtime.Object
import Runtime.Reference
import Runtime.Error
import Runtime.Global
import Runtime.Types
import Runtime.PropertyDescriptor
import Expr
import JSNum

-- ref 10.6
createArgumentsObject :: JSVal -> [String] -> [JSVal] -> EnvRec -> Strictness -> Runtime JSVal
createArgumentsObject func names args env strict =
  let len = JSNum (fromIntegral $ length args)
      thrower _  = raiseTypeError "Cannot access property"
      sthrower _ = thrower
  in do
    objectPrototype <- getGlobalObjectPrototype

    map <- newObject

    obj <- newObject >>= setClass "Arguments"
                     >>= objSetPrototype objectPrototype
                     >>= addOwnPropDesc "length" (dataPD (VNum len) True False True)



    forM_ (zip3 (names ++ repeat "") args [0..]) $ \(name, val, indx) -> do
      defineOwnProperty (show indx) (dataPD val True True True) False obj

      when (strict == NotStrict && name /= "") $
        let getter _       = getBindingValue name strict env
            setter _this a = setMutableBinding name a (strict == Strict) env
            desc           = accessorPD (Just getter) (Just setter) True True
        in void $ defineOwnProperty (show indx) desc False map

    case strict of
      NotStrict -> addOwnPropDesc "callee" (dataPD func True False True) obj
                     >>= setGetMethod (argGet map)
                     >>= setGetOwnPropertyMethod (argGetOwnProperty map)
                     >>= setDefineOwnPropertyMethod (argDefineOwnProperty map)
      Strict ->
        let prop = accessorPD (Just thrower) (Just sthrower) False False
        in do addOwnPropDesc "caller" prop obj
              addOwnPropDesc "callee" prop obj

    return (VObj obj)

argGet :: Shared JSObj -> String -> Shared JSObj -> Runtime JSVal
argGet map p obj = do
  objGetOwnProperty p map >>= \case
    Just _  -> objGetObj p map
    Nothing -> objGetObj p obj

argGetOwnProperty :: Shared JSObj -> String -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
argGetOwnProperty map p obj = do
  desc <- objGetOwnPropertyObj p obj
  case desc of
    Nothing -> return desc
    Just d -> do
      d' <- objGetOwnProperty p map
      case d' of
        Nothing -> return desc
        Just _  -> do
          val <- objGet p map
          return $ Just $ (d `mappend` valuePD val)

argDefineOwnProperty :: Shared JSObj -> String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool
argDefineOwnProperty map p desc throw obj = do
  isMapped <- objGetOwnProperty p map
  allowed  <- objDefineOwnPropertyObject p desc throw obj
  if not allowed
  then if throw
       then raiseTypeError $ "Cannot set arguments[" ++ p ++ "]"
       else return False
  else do
    when (isJust isMapped) $
      if isAccessorDescriptor (Just desc)
      then void $ objDelete p False map
      else do
        case propValue desc of
          Just v  -> objPut p v throw map
          Nothing -> return ()
        when (propIsWritable desc && propValue desc == Nothing) $
          void $ objDelete p False map
    return True



