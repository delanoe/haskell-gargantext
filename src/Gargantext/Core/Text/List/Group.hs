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

import Control.Lens (set, view)
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
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

class ToGroupedTree a b | a -> b where
  toGroupedTree :: GroupParams
                -> FlowCont Text FlowListScores
                -> a
                -> FlowCont Text (GroupedTreeScores b)

instance ToGroupedTree (Map Text (Set NodeId)) (Set NodeId)
  where
    toGroupedTree :: GroupParams
                  -> FlowCont Text FlowListScores
                  -> Map Text (Set NodeId)
                 -- -> Map Text (GroupedTreeScores (Set NodeId))
                  -> FlowCont Text (GroupedTreeScores (Set NodeId))
    toGroupedTree groupParams flc scores = {-view flc_scores-} flow2
        where
          flow1     = groupWithScores' flc scoring
          scoring t = fromMaybe Set.empty $ Map.lookup t scores

          flow2 = case (view flc_cont flow1) == Map.empty of
            True  -> flow1
            False -> groupWithStem' groupParams flow1

instance ToGroupedTree (Map Text Double) Double
  where
    toGroupedTree :: GroupParams
                  -> FlowCont Text FlowListScores
                  -> Map Text Double
                 -- -> Map Text (GroupedTreeScores (Set NodeId))
                  -> FlowCont Text (GroupedTreeScores Double)
    toGroupedTree groupParams flc scores = {-view flc_scores-} flow2
        where
          flow1     = groupWithScores' flc scoring
          scoring t = fromMaybe mempty $ Map.lookup t scores

          flow2 = case (view flc_cont flow1) == Map.empty of
            True  -> flow1
            False -> groupWithStem' groupParams flow1

------------------------------------------------------------------------
------------------------------------------------------------------------
-- 8<-- 8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--
-- | TODO To be removed
toGroupedText :: GroupedTextParams a b
              -> Map Text FlowListScores
              -> Map Text (Set NodeId)
              -> Map Stem (GroupedText Int)
toGroupedText groupParams scores =
  (groupWithStem groupParams) . (groupWithScores scores)


addListType :: Map Text ListType -> GroupedText a -> GroupedText a
addListType m g = set gt_listType (hasListType m g) g
  where
    hasListType :: Map Text ListType -> GroupedText a -> Maybe ListType
    hasListType m' (GroupedText _ label _ g' _ _ _) =
        List.foldl' (<>) Nothing
      $ map (\t -> Map.lookup t m')
      $ Set.toList
      $ Set.insert label g'
