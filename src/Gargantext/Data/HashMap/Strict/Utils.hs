module Gargantext.Data.HashMap.Strict.Utils where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Gargantext.Prelude


unionsWith :: (Foldable f, Eq k, Hashable k) => (a->a->a) -> f (HashMap k a) -> HashMap k a
unionsWith f = foldl' (HM.unionWith f) HM.empty

partition :: Hashable k => (a -> Bool) -> HashMap k a -> (HashMap k a, HashMap k a) 
partition = undefined


partitionWithKey :: Hashable k => (k -> a -> Bool) -> HashMap k a -> (HashMap k a, HashMap k a)
partitionWithKey = undefined 

findMax :: Hashable k => HashMap k a -> (k, a) 
findMax = undefined

-- getKeyWithMaxValue :: Hashable k => HashMap k a -> Maybe k
getKeyWithMaxValue :: Hashable k => HashMap k a -> [k]
getKeyWithMaxValue = undefined

