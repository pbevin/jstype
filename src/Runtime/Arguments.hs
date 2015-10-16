{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Runtime.Arguments (createArgumentsObject) where

import Control.Lens hiding (strict)
import Control.Monad (forM_, when, void)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Runtime.Object
import Runtime.Reference
import Runtime.Error
import Runtime.Global
import Runtime.Types
import Runtime.PropertyDescriptor
import Runtime.Shared
import Expr

makeParamMap :: [String] -> [JSVal] -> ArgsParameterMap
makeParamMap names args = go M.empty . reverse $ zip3 (map T.pack names ++ repeat "") args [0..]
  where
    go m [] = m
    go m ((n, a, i):rest)
      | n `M.member` m = go m rest
      | otherwise =
          let key = T.pack (show i)
              val = (n, a)
           in go (M.insert key val m) rest

-- ref 10.6
createArgumentsObject :: JSVal -> [String] -> [JSVal] -> EnvRec -> Strictness -> Runtime JSVal
createArgumentsObject func names args env strict =
  let len = length args
      thrower _  = raiseTypeError "Cannot access property"
      sthrower _ = thrower
      paramMap   = makeParamMap names args
  in do
    objectPrototype <- getGlobalObjectPrototype
    obj <- newObject >>= setClass "Arguments"
                     >>= objSetPrototype objectPrototype
                     -- >>= addOwnPropDesc "length" (dataPD (VNum len) True False True)
                     >>= setHostData (ArgumentsData func len paramMap env strict)





    -- forM_ (zip3 (names ++ repeat "") args [0..]) $ \(name, val, indx) -> do
    --   defineOwnProperty (T.pack $ show indx) (dataPD val True True True) False obj

    --   when (strict == NotStrict && name /= "") $
    --     let getter _       = getBindingValue (T.pack name) strict env
    --         setter _this a = setMutableBinding (T.pack name) a (strict == Strict) env
    --         desc           = accessorPD (Just getter) (Just setter) True True
    --     in void $ defineOwnProperty (T.pack $ show indx) desc False map

    case strict of
      NotStrict -> addOwnPropDesc "callee" (dataPD func True False True) obj
                     -- >>= setGetMethod argGet
                     >>= setGetOwnPropertyMethod argGetOwnProperty
                     -- >>= setDefineOwnPropertyMethod argDefineOwnProperty
      Strict ->
        let prop = accessorPD (Just thrower) (Just sthrower) False False
        in do addOwnPropDesc "caller" prop obj
              addOwnPropDesc "callee" prop obj
              setGetOwnPropertyMethod argGetOwnProperty obj

    return (VObj obj)

-- argGet :: Text -> Shared JSObj -> Runtime JSVal
-- argGet p obj = deref obj >>= \o ->
--   let ArgumentsData func len paramMap env strict = view objHostData o
--    in case M.lookup p paramMap of
--         Nothing       -> objGetObj p obj
--         Just (_, val) -> return val

argGetOwnProperty :: Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
argGetOwnProperty p obj = deref obj >>= \o ->
  let ArgumentsData func len paramMap env strict = view objHostData o
      length = VInt $ fromIntegral len

   in case p of
     "length" -> return . Just $ dataPD length True False True
     "caller" -> undefined
     "callee" -> undefined
     _ -> case M.lookup p paramMap of
            Nothing -> return Nothing
            Just (name, val) ->
              if strict == NotStrict && name /= ""
                then let getter _       = getBindingValue name strict env
                         setter _this a = setMutableBinding name a (strict == Strict) env
                      in return . Just $ accessorPD (Just getter) (Just setter) True True
                else return . Just $ dataPD val True True True





-- argGet :: Text -> Shared JSObj -> Runtime JSVal
-- argGet p obj = getMap obj >>= \map -> do
--   objGetOwnProperty p map >>= \case
--     Just _  -> objGetObj p map
--     Nothing -> objGetObj p obj

-- argGetOwnProperty :: Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
-- argGetOwnProperty p obj = getMap obj >>= \argMap -> do
--   desc <- objGetOwnPropertyObj p obj
--   case desc of
--     Nothing -> return desc
--     Just d -> do
--       d' <- argMap p -- objGetOwnProperty p map
--       case d' of
--         Nothing -> return desc
--         Just desc -> do
--           val <- objGet p map
--           return $ Just $ (d `mappend` valuePD val)

-- argDefineOwnProperty :: Text -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool
-- argDefineOwnProperty p desc throw obj = getMap obj >>= \argMap -> do
--   isMapped <- argMap p
--   allowed  <- objDefineOwnPropertyObject p desc throw obj
--   if not allowed
--   then if throw
--        then raiseTypeError $ "Cannot set arguments[" <> p <> "]"
--        else return False
--   else do
--     when (isJust isMapped) $
--       if isAccessorDescriptor (Just desc)
--       then return () -- void $ objDelete p False argMap
--       else do
--         case propValue desc of
--           Just v  -> return () -- objPut p v throw argMap
--           Nothing -> return ()
--         when (propIsWritable desc && propValue desc == Nothing) $
--           return () -- void $ objDelete p False argMap
--     return True



-- getMap :: Shared JSObj -> Runtime (Ident -> Runtime (Maybe (PropDesc JSVal)))
-- getMap obj = do
--   d <- view objHostData <$> deref obj
--   return (argLookup d)

-- argLookup :: HostData -> Ident -> Runtime (Maybe (PropDesc JSVal))
-- argLookup d name = undefined
