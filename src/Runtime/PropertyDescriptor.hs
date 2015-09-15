module Runtime.PropertyDescriptor ( propIsWritable
                                  , propIsEnumerable
                                  , propIsConfigurable
                                  , propValue
                                  , propGetter
                                  , propSetter
                                  , dataPD
                                  , dataPD'
                                  , valuePD
                                  , accessorPD
                                  , isDataDescriptor
                                  , isAccessorDescriptor
                                  , isGenericDescriptor
                                  , hasGetter
                                  , hasSetter
                                  , propIsUnwritable
                                  , setValue
                                  ) where

import qualified Data.Map as M
import Runtime.Types

-- ref 8.6.1
propIsWritable, propIsEnumerable, propIsConfigurable :: PropDesc a -> Bool
propIsWritable = flag "writable" False
propIsEnumerable = flag "enumerable" False
propIsConfigurable = flag "configurable" False

propValue :: PropDesc a -> Maybe a
propValue pd = case getPropertyOfType "value" pd of
  Just (PropValue v) -> Just v
  _ -> Nothing

propGetter :: PropDesc a -> Maybe (JSVal -> Runtime a)
propGetter pd = case getPropertyOfType "get" pd of
  Just (PropGetter s) -> Just s
  _ -> Nothing

propSetter :: PropDesc a -> Maybe (JSVal -> a -> Runtime ())
propSetter pd = case getPropertyOfType "set" pd of
  Just (PropSetter s) -> Just s
  _ -> Nothing

flag :: String -> Bool -> PropDesc a -> Bool
flag name ifNone pd = case getPropertyOfType name pd of
  Just (PropFlag b) -> b
  _ -> ifNone

propIsUnwritable :: PropDesc a -> Bool
propIsUnwritable pd = not (propIsWritable pd || propIsConfigurable pd)

valueToProp :: a -> PropDesc a
valueToProp a = dataPD a True True True

readOnlyProperty :: a -> PropDesc a
readOnlyProperty a = dataPD a False True True

-- ref 8.10.1, ref 8.10.2, ref 8.10.3
isDataDescriptor, isAccessorDescriptor, isGenericDescriptor :: Maybe (PropDesc a) -> Bool
isDataDescriptor Nothing = False
isDataDescriptor (Just d) = hasPropertyType "value" d || hasPropertyType "writable" d

isAccessorDescriptor Nothing = False
isAccessorDescriptor (Just d) = hasPropertyType "get" d || hasPropertyType "set" d

isGenericDescriptor Nothing = False
isGenericDescriptor d = not (isDataDescriptor d || isAccessorDescriptor d)

hasGetter :: PropDesc a -> Bool
hasGetter = hasPropertyType "get"

hasSetter :: PropDesc a -> Bool
hasSetter = hasPropertyType "set"

hasPropertyType :: String -> PropDesc a -> Bool
hasPropertyType name (PropDesc m) = M.member name m

getPropertyOfType :: String -> PropDesc a -> Maybe (Property a)
getPropertyOfType name (PropDesc m) = M.lookup name m

valuePD :: a -> PropDesc a
valuePD v = PropDesc $ M.fromList [ ("value", PropValue v) ]

setValue :: a -> PropDesc a -> PropDesc a
setValue v (PropDesc m) = PropDesc $ M.union (M.fromList [ ("value", PropValue v) ]) m

dataPD :: a -> Bool -> Bool -> Bool -> PropDesc a
dataPD v w e c = PropDesc $ M.fromList [ ("value", PropValue v),
                                         ("writable", PropFlag w),
                                         ("enumerable", PropFlag e),
                                         ("configurable", PropFlag c) ]

dataPD' :: Maybe a -> Bool -> Bool -> Bool -> PropDesc a
dataPD' (Just v) w e c = dataPD v w e c
dataPD' Nothing w e c = PropDesc $ M.fromList  [ ("writable", PropFlag w),
                                                 ("enumerable", PropFlag e),
                                                 ("configurable", PropFlag c) ]

accessorPD :: Maybe (JSVal -> Runtime a) -> Maybe (JSVal -> a -> Runtime ()) -> Bool -> Bool -> PropDesc a
accessorPD g s e c = PropDesc $ M.fromList $ maybeAttr "get" PropGetter g
                                          ++ maybeAttr "set" PropSetter s
                                          ++ [ ("enumerable", PropFlag e),
                                               ("configurable", PropFlag c) ]
  where maybeAttr :: String -> (b -> Property a) -> Maybe b -> [(String, Property a)]
        maybeAttr _ _ Nothing = []
        maybeAttr name f (Just a) = [(name, f a)]
