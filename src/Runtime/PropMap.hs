module Runtime.PropMap where

import qualified Data.Map as M

newtype PropMap k a = PropMap (M.Map k (a, Bool)) deriving (Show, Eq)

emptyPropMap :: PropMap k a
emptyPropMap = PropMap M.empty

propMapMember :: Ord k => k -> PropMap k a -> Bool
propMapMember k (PropMap m) = M.member k m

propMapLookup :: Ord k => k -> PropMap k a -> Maybe a
propMapLookup k (PropMap m) = fst <$> M.lookup k m

propMapInsert :: Ord k => k -> a -> PropMap k a -> PropMap k a
propMapInsert k a = propMapInsert' k a True

propMapInsert' :: Ord k => k -> a -> Bool -> PropMap k a -> PropMap k a
propMapInsert' k a d (PropMap m) = PropMap $ M.insert k (a, d) m

propMapDelete :: Ord k => k -> PropMap k a -> PropMap k a
propMapDelete k (PropMap m) = PropMap $ M.delete k m

propMapToList :: PropMap k a -> [(k, a)]
propMapToList (PropMap m) = map unwrap (M.toList m)
  where unwrap (k, (a, _)) = (k, a)

propMapFromList :: Ord k => [(k, a)] -> PropMap k a
propMapFromList = PropMap . M.fromList . map f
  where f (k,a) = (k, (a, False))

propMapKeys :: PropMap k a -> [k]
propMapKeys (PropMap m) = M.keys m
