module Gargantext.Data.HashMap.Strict.Utils where

import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HashMap

------------------------------------------------------------------------
unionsWith :: (Foldable f, Eq k, Hashable k) => (a->a->a) -> f (HashMap k a) -> HashMap k a
unionsWith f = foldl' (HashMap.unionWith f) HashMap.empty

------------------------------------------------------------------------
-- | Partition the map according to some predicate. The first map contains all
-- elements that satisfy the predicate, the second all elements that fail the
-- predicate.
partition :: (Ord k, Hashable k) => (a -> Bool) -> HashMap k a -> (HashMap k a, HashMap k a) 
partition p m = (HashMap.filter p m, HashMap.filter (not . p) m)

-- | Partition the map according to some predicate. The first map contains all
-- elements that satisfy the predicate, the second all elements that fail the
-- predicate.
partitionWithKey :: (Ord a, Hashable k) => (k -> a -> Bool) -> HashMap k a -> (HashMap k a, HashMap k a)
partitionWithKey p m = (HashMap.filterWithKey p m, HashMap.filterWithKey (\k -> not . p k) m)


------------------------------------------------------------------------
-- getKeyWithMaxValue :: Hashable k => HashMap k a -> Maybe k
getKeysOrderedByValueMaxFirst :: (Ord k, Hashable k, Ord a) => HashMap k a -> [k]
getKeysOrderedByValueMaxFirst m = go [] Nothing (HashMap.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

