{-|
Module      : Gargantext.Core.Text.List.WithScores
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Core.Text.List.Group.WithScores
  where

import Control.Lens (makeLenses, view, set)
import Data.Semigroup
import Data.Set (Set)
import Data.Map (Map)
import Data.Monoid (mempty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map


------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Main function
groupWithScores' :: FlowCont Text FlowListScores
                 -> (Text -> Set NodeId) -- Map Text (Set NodeId)
                 -> FlowCont Text (GroupedTextScores' (Set NodeId))
groupWithScores' flc _scores = FlowCont  groups orphans
  where
    groups  = toGroupedTextScores' $ view flc_scores flc
    -- parent/child relation is inherited from social lists

    orphans = (view flc_cont flc)
    -- orphans have been filtered already

------------------------------------------------------------------------
------------------------------------------------------------------------
toGroupedTextScores' :: Map Text FlowListScores
                     -> Map Parent (GroupedTextScores' (Set NodeId))
toGroupedTextScores' = toGroupedScores' . fromListScores'
------------------------------------------------------------------------
fromListScores' :: Map Text FlowListScores
                -> Map Parent GroupedWithListScores
fromListScores' = Map.fromListWith (<>) . (map fromScores') . Map.toList
  where
    fromScores' :: (Text, FlowListScores) -> (Text, GroupedWithListScores)
    fromScores' (t, fs) = case (keyWithMaxValue $ view fls_parents fs) of
      Nothing     -> ( t
                     , set gwls_listType (keyWithMaxValue $ view fls_listType fs) mempty
                     )
          -- Parent case: taking its listType, for now children Set is empty

      Just parent -> (parent, set gwls_children (Set.singleton t) mempty)
          -- We ignore the ListType of children for the parents' one
          -- added after and winner of semigroup actions


toGroupedScores' :: Map Parent GroupedWithListScores
                 -> Map Parent (GroupedTextScores' (Set NodeId))
toGroupedScores' = undefined
-- Map.map (\(GroupedWithListScores c l) -> GroupedTextScores l Set.empty c)

-- toGroupedTree :: GroupedW












--8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<--
-- TODO To be removed
data GroupedTextScores score =
  GroupedTextScores { _gts_listType :: !(Maybe ListType)
                    , _gts_score    :: score
                    , _gts_children :: !(Set Text)
                    } deriving (Show)
makeLenses 'GroupedTextScores
instance Semigroup a => Semigroup (GroupedTextScores a) where
  (<>) (GroupedTextScores  l1 s1 c1)
       (GroupedTextScores  l2 s2 c2)
      = GroupedTextScores (l1 <> l2)
                          (s1 <> s2)
                          (c1 <> c2)

-- | Main function
groupWithScores :: Map Text FlowListScores
                -> Map Text (Set NodeId)
                -> Map Text (GroupedTextScores (Set NodeId))
groupWithScores scores ms = orphans <> groups
  where
    groups = addScore ms
           $ fromGroupedScores
           $ fromListScores scores
    orphans = addIfNotExist scores ms

------------------------------------------------------------------------
addScore :: Map Text (Set NodeId)
         -> Map Text (GroupedTextScores (Set NodeId))
         -> Map Text (GroupedTextScores (Set NodeId))
addScore mapNs = Map.mapWithKey scoring
  where

    scoring k g = set gts_score ( Set.unions
                                $ catMaybes
                                $ map (\n -> Map.lookup n mapNs)
                                $ [k] <> (Set.toList $ view gts_children g)
                                ) g

addIfNotExist :: Map Text FlowListScores
              -> Map Text (Set NodeId)
              -> Map Text (GroupedTextScores (Set NodeId))
addIfNotExist mapSocialScores mapScores =
  foldl' (addIfNotExist' mapSocialScores) Map.empty $ Map.toList mapScores
    where
      addIfNotExist' mss m (t,ns) =
        case Map.lookup t mss of
          Nothing -> Map.alter (add ns) t m
          _       -> m

      add ns' Nothing = Just $ GroupedTextScores Nothing ns' Set.empty
      add _ _         = Nothing -- should not be present

------------------------------------------------------------------------
------------------------------------------------------------------------
fromGroupedScores :: Map Parent GroupedWithListScores
                  -> Map Parent (GroupedTextScores (Set NodeId))
fromGroupedScores = Map.map (\(GroupedWithListScores l c) -> GroupedTextScores l Set.empty c)

------------------------------------------------------------------------
fromListScores :: Map Text FlowListScores -> Map Parent GroupedWithListScores
fromListScores = Map.fromListWith (<>) . (map fromScores') . Map.toList
  where
    fromScores' :: (Text, FlowListScores) -> (Text, GroupedWithListScores)
    fromScores' (t, fs) = case (keyWithMaxValue $ view fls_parents fs) of
      Nothing     -> (t, set gwls_listType (keyWithMaxValue $ view fls_listType fs) mempty)
          -- Parent case: taking its listType, for now children Set is empty
 
      Just parent -> (parent, set gwls_children (Set.singleton t) mempty)
          -- We ignore the ListType of children for the parents' one
          -- added after and winner of semigroup actions


--8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<-- -8<--
