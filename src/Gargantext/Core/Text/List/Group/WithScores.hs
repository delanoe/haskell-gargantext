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

import Control.Lens (makeLenses, over, view)
import Data.Semigroup
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Core.Types (ListType(..)) -- (MasterCorpusId, UserCorpusId)
import Gargantext.Database.Admin.Types.Node (NodeId)
-- import Gargantext.Core.Text.List.Learn (Model(..))
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
        GroupedWithListScores (c1 <> c2) (l1 <> l2)

------
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

------------------------------------------------------------------------
-- | Main function
groupWithScores :: Map Text FlowListScores
                -> Map Text (Set NodeId)
                -> Map Text (GroupedTextScores (Set NodeId))
groupWithScores scores ms = foldl' (addScore scores) start (Map.toList ms)
  where
    start = fromGroupedScores $ fromListScores scores


-- | Add scores depending on being either parent or child or orphan
addScore :: Map Text FlowListScores
         -> Map Text (GroupedTextScores (Set NodeId))
         -> (Text, Set NodeId)
         -> Map Text (GroupedTextScores (Set NodeId))
addScore scores ms (t, ns) = Map.alter (isParent ns) t ms
  where
    -- is parent case
    isParent ns' (Just (GroupedTextScores l s c)) = let ns'' = ns' <> s in Just (GroupedTextScores l ns'' c)

    -- is either child or orphan case
    isParent ns' Nothing = Just $ GroupedTextScores Nothing ns' Set.empty
{- case Map.lookup t scores of
      -- is child case
      Just fls -> case keyWithMaxValue $ view fls_parents fls of
        Just parent -> over gts_score (<> ns') <$> Map.lookup parent ms
        Nothing     -> panic "[G.C.T.G.WS.addScore] Should not happen"
      -- is Orphan case
      Nothing -> Just $ GroupedTextScores Nothing ns' Set.empty
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

