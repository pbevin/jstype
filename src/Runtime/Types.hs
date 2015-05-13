{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Runtime.Types where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.IORef
import qualified Data.Map as M
import Control.Applicative

import Expr

data JSVal = VNum JSNum
           | VStr String
           | VBool Bool
           | VRef JSRef
           | VUndef
           | VObj JSObj
           | VMap (M.Map Ident JSVal)
           | VNative (JSVal -> [JSVal] -> JSRuntime JSVal)
           | VPrim PrimitiveFunction
           | VFormalParams [Ident]
           | VFuncBody [Statement]
           | JSErrorObj JSVal
           | VEnv JSEnv

instance Show JSVal where
  show (VNum a) = show a
  show (VStr a) = show a
  show (VBool a) = show a
  show (VRef _) = "(reference)"
  show VUndef = "undefined"
  show (VObj _) = "[Object object]"
  show (VMap _) = "(map)"
  show (VNative _) = "(native function)"
  show (JSErrorObj a) = "JSError(" ++ show a ++ ")"

data JSObj = JSObj { objInternal :: M.Map String JSVal }

data JSRef = JSRef { getBase :: JSVal, getReferencedName :: String, isStrictReference :: Bool }

data JSType = TypeUndefined
            | TypeNull
            | TypeBoolean
            | TypeNumber
            | TypeString
            | TypeObject
            | TypeFunction
            | TypeReference
            | TypeList
            | TypeOther String
            deriving (Show, Eq)

typeof :: JSVal -> JSType
typeof v = case v of
  VNum _ -> TypeNumber
  VStr _ -> TypeString
  VBool _ -> TypeBoolean
  VRef  _ -> TypeReference
  VUndef  -> TypeUndefined
  VNative _ -> TypeFunction
  VObj _  -> TypeObject
  _       -> error $ "No idea what type " ++ show v ++ " is..."

instance Eq JSVal where
  VNum a == VNum b = a == b
  VStr a == VStr b = a == b
  a == b = error $ "Can't compare " ++ show a ++ " and " ++ show b

type JSOutput = String
type JSError = String
type JSEnv = IORef (M.Map Ident (IORef JSVal))
type PrimitiveFunction = JSVal -> [JSVal] -> JSRuntime JSVal

newtype JSRuntime a = JS {
  unJS :: ExceptT JSError (WriterT String IO) a
} deriving (Monad, MonadIO, MonadWriter String, MonadError JSError, Functor, Applicative)

