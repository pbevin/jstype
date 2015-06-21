{-# LANGUAGE LambdaCase #-}

module Builtins.Array.Sort (arraySort) where

import Control.Monad (void)
import qualified Data.Sequence as Seq
import Data.Foldable
import Safe
import Runtime

-- ref 15.4.4.11
arraySort :: JSFunction
arraySort this args =
  let comparefn = sortCompare (headMay args)
  in do
    obj <- toObject this
    len <- toInt =<< objGet "length" obj
    arr <- slurp len obj
    sorted <- sortByM comparefn arr
    spew len obj sorted

    return (VObj obj)

  where

    sortCompare :: Maybe JSVal -> MComparator Runtime (Maybe JSVal)
    sortCompare comparefn j k =
      case (comparefn, j, k) of
        (_, Nothing, Nothing)     -> return EQ
        (_, Nothing, Just _)      -> return GT
        (_, Just _, Nothing)      -> return LT
        (Nothing, Just x, Just y) -> do
          xs <- toString x
          ys <- toString y
          return (compare xs ys)
        (Just f, Just x, Just y)  -> do
          isCallable f >>= \case
            Nothing -> sortCompare Nothing j k
            Just fn -> do
              v <- toNumber =<< fn VUndef [x, y]
              return (compare v 0)

          

    slurp :: Int -> Shared JSObj -> Runtime [Maybe JSVal]
    slurp len obj = sequence $ toList $ Seq.fromFunction len readFromObj
      where
        readFromObj :: Int -> Runtime (Maybe JSVal)
        readFromObj i = objGetMaybe (show i) obj

    spew :: Int -> Shared JSObj -> [Maybe JSVal] -> Runtime ()
    spew len obj vals = do
      forM_ (zip [0..] vals) $ \(i,v) -> do
        case v of
          Nothing  -> void $ objDelete (show i) True obj
          Just val -> objPut (show i) val True obj


-- https://unknownparallel.wordpress.com/2012/07/03/using-monadic-effects-to-reverse-a-merge-sort/
type MComparator m a = a -> a -> m Ordering

sortByM :: (Monad m, Functor m) => MComparator m a -> [a] -> m [a]
sortByM _ []   = return []
sortByM _ [x]  = return [x]
sortByM cmp xs = do
  let (ys, zs) = partition xs
  ys' <- sortByM cmp ys
  zs' <- sortByM cmp zs
  merge ys' zs'
  where merge [] bs = return bs
        merge as [] = return as
        merge (a:as) (b:bs) = do
          comparison <- cmp a b
          case comparison of
            LT -> (a:) <$> merge as (b:bs)
            _  -> (b:) <$> merge (a:as) bs
        partition as = splitAt (length as `quot` 2) as
