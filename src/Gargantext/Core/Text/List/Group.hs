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

{-
DM.foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
-}

setScoresWith :: (Ord a, Ord b)
              =>  (Text -> (GroupedTreeScores a) -> (GroupedTreeScores b))
              -> Map Text (GroupedTreeScores a)
              -> Map Text (GroupedTreeScores b)
setScoresWith = Map.mapWithKey

{-
Map.foldlWithKey (\k v ->
                                   {- over gts'_children (setScoresWith fun)
                                          $ over gts'_score    (fun k)
                                          -}
                                          set gts'_score Set.empty -- (fun k)
                                          v
                                   ) mempty m
-}

------------------------------------------------------------------------
