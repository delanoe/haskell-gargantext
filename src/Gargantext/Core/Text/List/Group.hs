{-|
Module      : Gargantext.Core.Text.List.Group
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}

module Gargantext.Core.Text.List.Group
  where

import Control.Lens (set, view, over)
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mempty)
import Data.Text (Text)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Group.WithStem
import Gargantext.Core.Text.List.Group.WithScores
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List

------------------------------------------------------------------------
-- | TODO add group with stemming
toGroupedTree :: (Ord a, Monoid a, GroupWithStem a)
              => GroupParams
              -> FlowCont Text FlowListScores
              -> Map Text a
             -- -> Map Text (GroupedTreeScores (Set NodeId))
              -> FlowCont Text (GroupedTreeScores a)
toGroupedTree groupParams flc scores = {-view flc_scores-} flow2
    where
      flow1     = groupWithScores' flc scoring
      scoring t = fromMaybe mempty $ Map.lookup t scores

      flow2 = case (view flc_cont flow1) == Map.empty of
        True  -> flow1
        False -> groupWithStem' groupParams flow1



------------------------------------------------------------------------
setScoresWithMap :: (Ord a, Ord b, Monoid b) => Map Text b
                 -> Map Text (GroupedTreeScores a)
                 -> Map Text (GroupedTreeScores b)
setScoresWithMap m = setScoresWith (score m)
  where
    score m t = case Map.lookup t m of
      Nothing -> mempty
      Just  r -> r

setScoresWith :: (Ord a, Ord b)
              => (Text -> b)
              -> Map Text (GroupedTreeScores a)
              -> Map Text (GroupedTreeScores b)
{-
-- | This Type level lenses solution does not work
setScoresWith f = Map.mapWithKey (\k v -> over gts'_children (setScoresWith f)
                                       $  set  gts'_score    (f k) v
                                 )
-}
setScoresWith f = Map.mapWithKey (\k v -> v { _gts'_score    = f k
                                            , _gts'_children = setScoresWith f
                                                             $ view gts'_children v
                                            }
                                 )


------------------------------------------------------------------------
