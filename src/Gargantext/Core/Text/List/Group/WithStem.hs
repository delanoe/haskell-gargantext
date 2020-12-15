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

import Control.Lens (view, over)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid (mempty)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams.Types
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Social.Patch
import Gargantext.Core.Text.Terms.Mono.Stem (stem)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Map.Strict.Patch as PatchMap
import qualified Data.Patch.Class as Patch (Replace(..))
import qualified Data.Set  as Set
import qualified Data.Text as Text

------------------------------------------------------------------------
addScoreStem :: GroupParams
             -> Set NgramsTerm
             -> FlowCont Text FlowListScores
             -> FlowCont Text FlowListScores
addScoreStem groupParams ngrams fl = foldl' addScorePatch fl 
                                   $ stemPatches groupParams ngrams

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
--------------------------------------------------------------------
stemPatches :: GroupParams
           -> Set NgramsTerm
           -> [(NgramsTerm, NgramsPatch)]
stemPatches groupParams = patches
                        . Map.fromListWith (<>)
                        . map (\ng@(NgramsTerm t) -> ( groupWith groupParams t
                                                     , Set.singleton ng)
                              )
                        . Set.toList

-- | For now all NgramsTerm which have same stem
-- are grouped together
-- Parent is taken arbitrarly for now (TODO use a score like occ)
patches :: Map Stem (Set NgramsTerm)
            -> [(NgramsTerm, NgramsPatch)]
patches = catMaybes . map patch . Map.elems

patch :: Set NgramsTerm
           -> Maybe (NgramsTerm, NgramsPatch)
patch s = case Set.size s > 1 of
  False -> Nothing
  True  -> do
    let ngrams = Set.toList s
    parent   <- headMay ngrams
    let children = List.tail ngrams
    pure (parent, toNgramsPatch children)
    
toNgramsPatch :: [NgramsTerm] -> NgramsPatch
toNgramsPatch children = NgramsPatch children' Patch.Keep
  where
    children' :: PatchMSet NgramsTerm
    children' = PatchMSet
              $ fst
              $ PatchMap.fromList
              $ List.zip children (List.cycle [addPatch])

------------------------------------------------------------------------
-- 8< - 8< - 8< - 8< - 8< - 8< - 8< - 8< - 8< - 8< - 8< - 8< - 8< - 8< -- 
-- TODO remove below
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

{-
-- | TODO fixme
mergeWith_a :: (Text -> Text)
          -> FlowCont Text (GroupedTreeScores a)
          -> FlowCont Text (GroupedTreeScores a)
mergeWith_a fun flc = FlowCont scores mempty
  where

    scores :: Map Text (GroupedTreeScores a)
    scores = foldl' (alter (mapStems scores')) scores' cont'
      where
        scores' = view flc_scores flc
        cont'   = Map.toList $ _flc_cont flc

    -- TODO insert at the right place in group hierarchy
    -- adding as child of the parent for now
    alter :: Map Stem Text
          -> Map Text (GroupedTreeScores a)
          -> (Text, GroupedTreeScores a)
          -> Map Text (GroupedTreeScores a)
    alter st target (t,g) = case Map.lookup t st of
      Nothing -> Map.alter (alter' (t,g)) t  target
      Just t' -> Map.alter (alter' (t,g)) t' target

    alter' (_t,g) Nothing   = Just g
    alter' ( t,g) (Just g') = Just $ over gts'_children
                                   ( Map.union (Map.singleton t g))
                                   g'

    mapStems :: Map Text (GroupedTreeScores a)
             -> Map Stem Text
    mapStems = (Map.fromListWith (<>)) . List.concat . (map mapStem) . Map.toList

    mapStem :: (Text, GroupedTreeScores a)
            -> [(Stem, Text)]
    mapStem (s,g) = parent : children
      where
        parent   = (fun s, s)
        children = List.concat $ map mapStem (Map.toList $ view gts'_children g)
-}



