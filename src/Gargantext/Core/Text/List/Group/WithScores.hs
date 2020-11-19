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
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Gargantext.Core.Types (ListType(..)) -- (MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
-- import Gargantext.Core.Text.List.Learn (Model(..))
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Social.Scores
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map

------------------------------------------------------------------------
-- | Main Types
data GroupedWithListScores =
  GroupedWithListScores { _gwls_children :: !(Set Text)
                        , _gwls_listType :: !(Maybe ListType)
                        } deriving (Show)
makeLenses ''GroupedWithListScores
instance Semigroup GroupedWithListScores where
  (<>) (GroupedWithListScores c1 l1)
       (GroupedWithListScores c2 l2) = 
        GroupedWithListScores (c1 <> c2)
                              (l1 <> l2)

------
-- To be removed
data GroupedTextScores score =
  GroupedTextScores { _gts_listType :: !(Maybe ListType)
                    , _gts_score    :: score
                    , _gts_children :: !(Set Text)
                    } deriving (Show)
makeLenses 'GroupedTextScores
instance Semigroup a => Semigroup (GroupedTextScores a) where
  (<>) (GroupedTextScores l1 s1 c1)
       (GroupedTextScores l2 s2 c2)
    = GroupedTextScores (l1 <> l2) (s1 <> s2) (c1 <> c2)

------
-- | Tree of GroupedTextScores
data GroupedTextScores' score =
  GroupedTextScores' { _gts'_listType :: !(Maybe ListType)
                     , _gts'_score    :: score
                     , _gts'_children :: !(Set (GroupedTextScores' score))
                     } deriving (Show, Ord, Eq)
makeLenses 'GroupedTextScores'
instance (Semigroup a, Ord a) => Semigroup (GroupedTextScores' a) where
  (<>) (GroupedTextScores' l1 s1 c1)
       (GroupedTextScores' l2 s2 c2)
    = GroupedTextScores' (l1 <> l2) (s1 <> s2) (c1 <> c2)


------------------------------------------------------------------------
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
{-
toGroupedTextScores' :: Map Parent GroupedWithListScores
                     -> Map Text (Set NodeId)
                     -> Map Parent (GroupedTextScores' (Set NodeId))
toGroupedTextScores' par datas = undefined
-}

------------------------------------------------------------------------
fromGroupedScores :: Map Parent GroupedWithListScores
                  -> Map Parent (GroupedTextScores (Set NodeId))
fromGroupedScores = Map.map (\(GroupedWithListScores c l) -> GroupedTextScores l Set.empty c)

------------------------------------------------------------------------
fromListScores :: Map Text FlowListScores -> Map Parent GroupedWithListScores
fromListScores = Map.fromListWith (<>) . (map fromScores') . Map.toList
  where
    fromScores' :: (Text, FlowListScores) -> (Text, GroupedWithListScores)
    fromScores' (t, fs) = case (keyWithMaxValue $ view fls_parents fs) of
      Nothing     -> (t, GroupedWithListScores Set.empty (keyWithMaxValue $ view fls_listType fs))
          -- Parent case: taking its listType, for now children Set is empty
 
      Just parent -> (parent, GroupedWithListScores (Set.singleton t) Nothing)
          -- We ignore the ListType of children for the parents' one
          -- added after and winner of semigroup actions

