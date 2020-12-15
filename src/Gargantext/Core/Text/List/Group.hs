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

import Control.Lens (view)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mempty)
import Data.Text (Text)
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Group.WithStem
import Gargantext.Core.Text.List.Group.WithScores
import Gargantext.Prelude
import qualified Data.Map  as Map

------------------------------------------------------------------------
toGroupedTree :: (Ord a, Monoid a, GroupWithStem a)
              => FlowCont Text FlowListScores
              -> Map Text a
              -> FlowCont Text (GroupedTreeScores a)
toGroupedTree flc scores =
  groupWithScores' flc scoring
    where
      scoring t = fromMaybe mempty $ Map.lookup t scores


------------------------------------------------------------------------
setScoresWithMap :: (Ord a, Ord b, Monoid b) => Map Text b
                 -> Map Text (GroupedTreeScores a)
                 -> Map Text (GroupedTreeScores b)
setScoresWithMap m = setScoresWith (score m)
  where
    score m' t = case Map.lookup t m' of
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
