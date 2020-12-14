module Gargantext.Data.HashMap.Strict.Utils where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Gargantext.Prelude

unionsWith :: (Foldable f, Eq k, Hashable k) => (a->a->a) -> f (HashMap k a) -> HashMap k a
unionsWith f = foldl' (HM.unionWith f) HM.empty
