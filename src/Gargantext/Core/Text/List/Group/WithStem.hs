{-|
Module      : Gargantext.Core.Text.List.Group.WithStem
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

module Gargantext.Core.Text.List.Group.WithStem
  where

import Control.Lens (makeLenses, view, over)
import Data.Set (Set)
import Data.Map (Map)
import Data.Monoid (mempty)
import Data.Text (Text)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text (size)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Core.Text.List.Group.WithScores
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Prelude
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List
import qualified Data.Text as Text

------------------------------------------------------------------------
-- | Main Types
data StopSize = StopSize {unStopSize :: !Int}
  deriving (Eq)

-- | TODO: group with 2 terms only can be
-- discussed. Main purpose of this is offering
-- a first grouping option to user and get some
-- enriched data to better learn and improve that algo
data GroupParams = GroupParams { unGroupParams_lang     :: !Lang
                               , unGroupParams_len      :: !Int
                               , unGroupParams_limit    :: !Int
                               , unGroupParams_stopSize :: !StopSize
                               }
                 | GroupIdentity
  deriving (Eq)

------------------------------------------------------------------------
class GroupWithStem a where
  groupWithStem' :: GroupParams
                -> FlowCont Text (GroupedTreeScores a)
                -> FlowCont Text (GroupedTreeScores a)

-- TODO factorize groupWithStem_*
instance GroupWithStem (Set NodeId) where
  groupWithStem' = groupWithStem_SetNodeId

instance GroupWithStem Double where
  groupWithStem' = groupWithStem_Double

------------------------------------------------------------------------
groupWith :: GroupParams
            -> Text
            -> Text
groupWith GroupIdentity  = identity
groupWith (GroupParams l _m _n _) =
                    Text.intercalate " "
                  . map (stem l)
                  -- . take n
                  . List.sort
                  -- . (List.filter (\t -> Text.length t > m))
                  . Text.splitOn " "
                  . Text.replace "-" " "
------------------------------------------------------------------------
groupWithStem_SetNodeId :: GroupParams
               -> FlowCont Text (GroupedTreeScores (Set NodeId))
               -> FlowCont Text (GroupedTreeScores (Set NodeId))
groupWithStem_SetNodeId g flc
    | g == GroupIdentity = FlowCont ( (<>)
                                      (view flc_scores flc)
                                      (view flc_cont   flc)
                                    ) mempty
    | otherwise = mergeWith (groupWith g) flc

groupWithStem_Double :: GroupParams
                     -> FlowCont Text (GroupedTreeScores Double)
                     -> FlowCont Text (GroupedTreeScores Double)
groupWithStem_Double g flc
    | g == GroupIdentity = FlowCont ( (<>)
                                      (view flc_scores flc)
                                      (view flc_cont   flc)
                                    ) mempty
    | otherwise = mergeWith_Double (groupWith g) flc




-- | MergeWith : with stem, we always have an answer
-- if Maybe lems then we should add it to continuation
mergeWith :: (Text -> Text)
          -> FlowCont Text (GroupedTreeScores (Set NodeId))
          -> FlowCont Text (GroupedTreeScores (Set NodeId))
mergeWith fun flc = FlowCont scores mempty
  where

    scores :: Map Text (GroupedTreeScores (Set NodeId))
    scores = foldl' (alter (mapStems scores')) scores' cont'
      where
        scores' = view flc_scores flc
        cont'   = Map.toList $ view flc_cont flc

    -- TODO insert at the right place in group hierarchy
    -- adding as child of the parent for now
    alter :: Map Stem Text
          -> Map Text (GroupedTreeScores (Set NodeId))
          -> (Text, GroupedTreeScores (Set NodeId))
          -> Map Text (GroupedTreeScores (Set NodeId))
    alter st target (t,g) = case Map.lookup t st of
      Nothing -> Map.alter (alter' (t,g)) t  target
      Just t' -> Map.alter (alter' (t,g)) t' target

    alter' (_t,g) Nothing   = Just g
    alter' ( t,g) (Just g') = Just $ over gts'_children
                                   ( Map.union (Map.singleton t g))
                                   g'

    mapStems :: Map Text (GroupedTreeScores (Set NodeId))
             -> Map Stem Text
    mapStems = (Map.fromListWith (<>)) . List.concat . (map mapStem) . Map.toList

    mapStem :: (Text, GroupedTreeScores (Set NodeId))
            -> [(Stem, Text)]
    mapStem (s,g) = parent : children
      where
        parent   = (fun s, s)
        children = List.concat $ map mapStem (Map.toList $ view gts'_children g)


-- | MergeWith : with stem, we always have an answer
-- if Maybe lems then we should add it to continuation
mergeWith_Double :: (Text -> Text)
          -> FlowCont Text (GroupedTreeScores Double)
          -> FlowCont Text (GroupedTreeScores Double)
mergeWith_Double fun flc = FlowCont scores mempty
  where

    scores :: Map Text (GroupedTreeScores Double)
    scores = foldl' (alter (mapStems scores')) scores' cont'
      where
        scores' = view flc_scores flc
        cont'   = Map.toList $ view flc_cont flc

    -- TODO insert at the right place in group hierarchy
    -- adding as child of the parent for now
    alter :: Map Stem Text
          -> Map Text (GroupedTreeScores Double)
          -> (Text, GroupedTreeScores Double)
          -> Map Text (GroupedTreeScores Double)
    alter st target (t,g) = case Map.lookup t st of
      Nothing -> Map.alter (alter' (t,g)) t  target
      Just t' -> Map.alter (alter' (t,g)) t' target

    alter' (_t,g) Nothing   = Just g
    alter' ( t,g) (Just g') = Just $ over gts'_children
                                   ( Map.union (Map.singleton t g))
                                   g'

    mapStems :: Map Text (GroupedTreeScores Double)
             -> Map Stem Text
    mapStems = (Map.fromListWith (<>)) . List.concat . (map mapStem) . Map.toList

    mapStem :: (Text, GroupedTreeScores Double)
            -> [(Stem, Text)]
    mapStem (s,g) = parent : children
      where
        parent   = (fun s, s)
        children = List.concat $ map mapStem (Map.toList $ view gts'_children g)



