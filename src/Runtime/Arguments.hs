{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Runtime.Arguments (createArgumentsObject) where

import Control.Lens hiding (strict)
import Control.Monad (forM_, when, void)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
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


makeParamMap :: [String] -> [JSVal] -> Strictness -> ArgsParameterMap
makeParamMap names vals strict =
  let args = zip3(map T.pack names ++ repeat "") vals [0..]
      mkAssoc xs = snd $ foldl step (S.empty, []) (reverse xs)
      step :: (S.Set Text, [(Text, ArgsParamType)]) -> (Text, JSVal, Int) -> (S.Set Text, [(Text, ArgsParamType)])
      step (seen, as) (name, val, i) =
        let elem = if strict == Strict || (name == "" || name `S.member` seen)
                     then ArgUnbound val
                     else ArgBound name
         in (S.insert name seen, (T.pack $ show i, elem) : as)
   in M.fromList (mkAssoc args)

-- ref 10.6
createArgumentsObject :: JSVal -> [String] -> [JSVal] -> EnvRec -> Strictness -> Runtime JSVal
createArgumentsObject func names args env strict =
  let len = length args
      thrower _  = raiseTypeError "Cannot access property"
      sthrower _ = thrower
      extraProps = M.fromList [("caller", ArgMeta func), ("callee", ArgMeta func), ("length", ArgMeta (VInt $ fromIntegral len))]
      paramMap   = makeParamMap names args strict `M.union` extraProps
  in do
    objectPrototype <- getGlobalObjectPrototype
    obj <- newObject >>= setClass "Arguments"
                     >>= objSetPrototype objectPrototype
                     >>= setHostData (ArgumentsData func len paramMap env strict)
                     >>= setGetOwnPropertyMethod argGetOwnProperty
                     >>= setDeleteMethod argDelete

    when (strict == Strict) $
      let prop = accessorPD (Just thrower) (Just sthrower) False False
       in void $ do addOwnPropDesc "caller" prop obj
                    addOwnPropDesc "callee" prop obj

    return (VObj obj)


argGetOwnProperty :: Text -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
argGetOwnProperty p objRef = deref objRef >>= \obj -> do
   objGetOwnPropertyObj p objRef >>= \case
     Just prop -> return (Just prop)
     Nothing ->
      let ArgumentsData func len paramMap env strict = view objHostData obj
          length = VInt $ fromIntegral len

       in case M.lookup p paramMap of
         Nothing -> return Nothing
         Just (ArgMeta val)    -> return . Just $ dataPD val True False True
         Just (ArgUnbound val) -> return . Just $ dataPD val True True True
         Just (ArgBound name)  ->
           let getter _       = getBindingValue name strict env
               setter _this a = setMutableBinding name a (strict == Strict) env
            in return . Just $ accessorPD (Just getter) (Just setter) True True

argDelete :: Text -> Bool -> Shared JSObj -> Runtime JSVal
argDelete p throw objRef = deref objRef >>= \obj -> do
  result <- objDeleteObject p throw objRef
  let ArgumentsData func len paramMap env strict = view objHostData obj
      newParamMap = M.delete p paramMap
   in do
     setHostData (ArgumentsData func len newParamMap env strict) objRef
     return result
