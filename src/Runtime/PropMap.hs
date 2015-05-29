module Runtime.PropMap where

import qualified Data.Map as M

newtype PropMap k a = PropMap (M.Map k a) deriving (Show, Eq)

emptyPropMap :: PropMap k a
emptyPropMap = PropMap M.empty

propMapMember :: Ord k => k -> PropMap k a -> Bool
propMapMember k (PropMap m) = M.member k m

propMapLookup :: Ord k => k -> PropMap k a -> Maybe a
propMapLookup k (PropMap m) = M.lookup k m

propMapInsert :: Ord k => k -> a -> PropMap k a -> PropMap k a
propMapInsert k a (PropMap m) = PropMap $ M.insert k a m

propMapDelete :: Ord k => k -> PropMap k a -> PropMap k a
propMapDelete k (PropMap m) = PropMap $ M.delete k m

propMapToList :: PropMap k a -> [(k, a)]
propMapToList (PropMap m) = M.toList m

propMapFromList :: Ord k => [(k, a)] -> PropMap k a
propMapFromList = PropMap . M.fromList