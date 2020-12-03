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
{-# LANGUAGE InstanceSigs           #-}

module Gargantext.Core.Text.List.Group.WithScores
  where

import Control.Lens (view, set, over)
import Data.Semigroup
import Data.Map (Map)
import Data.Monoid (Monoid, mempty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Prelude
import qualified Data.Map  as Map

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Main function
groupWithScores' :: (Eq a, Ord a, Monoid a)
                => FlowCont Text FlowListScores
                -> (Text -> a) -- Map Text (a)
                -> FlowCont Text (GroupedTreeScores (a))
groupWithScores' flc scores = FlowCont  groups orphans
  where
    -- parent/child relation is inherited from social lists
    groups  = toGroupedTree
            $ toMapMaybeParent scores
            $ view flc_scores flc

    -- orphans should be filtered already
    orphans = toGroupedTree
            $ toMapMaybeParent scores
            $ view flc_cont flc
------------------------------------------------------------------------
toMapMaybeParent :: (Eq a, Ord a, Monoid a)
                 => (Text -> a)
                 -> Map Text FlowListScores
                 -> Map (Maybe Parent) (Map Text (GroupedTreeScores (a)))
toMapMaybeParent f =  Map.fromListWith (<>)
                   . (map (fromScores'' f))
                   .  Map.toList

fromScores'' :: (Eq a, Ord a, Monoid a)
             => (Text -> a)
             -> (Text, FlowListScores)
             -> (Maybe Parent, Map Text (GroupedTreeScores (a)))
fromScores'' f' (t, fs) = ( maybeParent
                          , Map.fromList [( t, set gts'_score (f' t)
                                             $ set gts'_listType maybeList mempty
                                         )]
                          )
    where
     maybeParent = keyWithMaxValue $ view fls_parents  fs
     maybeList   = keyWithMaxValue $ view fls_listType fs

toGroupedTree :: Eq a
              => Map (Maybe Parent) (Map Text (GroupedTreeScores (a)))
              -> Map Parent (GroupedTreeScores (a))
toGroupedTree m = case Map.lookup Nothing m of
  Nothing  -> mempty
  Just  m' -> toGroupedTree' m m'


toGroupedTree' :: Eq a => Map (Maybe Parent) (Map Text (GroupedTreeScores (a)))
               -> (Map Text (GroupedTreeScores (a)))
               ->  Map Parent (GroupedTreeScores (a))
toGroupedTree' m notEmpty
  | notEmpty == mempty = mempty
  | otherwise = Map.mapWithKey (addGroup m) notEmpty
    where
      addGroup m' k v = over gts'_children ( (toGroupedTree' m')
                                           . (Map.union ( fromMaybe mempty
                                                        $ Map.lookup (Just k) m'
                                                        )
                                             )
                                           )
                                           v







