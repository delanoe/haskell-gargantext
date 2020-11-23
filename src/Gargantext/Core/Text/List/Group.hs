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


module Gargantext.Core.Text.List.Group
  where

import Control.Lens (set, view)
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
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
toGroupedText :: GroupedTextParams a b
              -> Map Text FlowListScores
              -> Map Text (Set NodeId)
              -> Map Stem (GroupedText Int)
toGroupedText groupParams scores =
  (groupWithStem groupParams) . (groupWithScores scores)

-- | TODO add group with stemming
toGroupedTreeText :: GroupedTextParams a b
              -> FlowCont Text FlowListScores
              -> Map Text (Set NodeId)
              -> Map Text (GroupedTreeScores (Set NodeId))
toGroupedTreeText _groupParams flc scores = view flc_scores flow1
    where
      flow1 = groupWithScores' flc scoring
      scoring t = fromMaybe Set.empty $ Map.lookup t scores

{-
      flow2 = case flc_cont flow1 == Set.empty of
        True  -> view flc_scores flow1
        False -> groupWithStem' groupParams flow1


      groupWithStem' :: GroupedTextParams a b
                     -> FlowCont Text (GroupedTreeScores (Set NodeId))
                     -> FlowCont Text (GroupedTreeScores (Set NodeId))
      groupWithStem' _groupParams = identity
-}

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | TODO To be removed
addListType :: Map Text ListType -> GroupedText a -> GroupedText a
addListType m g = set gt_listType (hasListType m g) g
  where
    hasListType :: Map Text ListType -> GroupedText a -> Maybe ListType
    hasListType m' (GroupedText _ label _ g' _ _ _) =
        List.foldl' (<>) Nothing
      $ map (\t -> Map.lookup t m')
      $ Set.toList
      $ Set.insert label g'
