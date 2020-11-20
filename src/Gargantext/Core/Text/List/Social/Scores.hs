{-|
Module      : Gargantext.Core.Text.List.Social.Scores
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

module Gargantext.Core.Text.List.Social.Scores
  where

import Control.Lens
import Data.Map (Map)
import Data.Monoid (mempty)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Prelude
import qualified Data.Map   as Map
import qualified Data.Set   as Set

------------------------------------------------------------------------
-- | Generates Score from list of Map Text NgramsRepoElement
toFlowListScores :: KeepAllParents
                 ->  FlowCont Text FlowListScores
                 -> [Map Text NgramsRepoElement]
                 ->  FlowCont Text FlowListScores
toFlowListScores k flc_origin = foldl' (toFlowListScores_Level1 k flc_origin) mempty

  where

    toFlowListScores_Level1 :: KeepAllParents
                     -> FlowCont Text FlowListScores
                     -> FlowCont Text FlowListScores
                     -> Map Text NgramsRepoElement
                     -> FlowCont Text FlowListScores
    toFlowListScores_Level1 k' flc_origin' flc_dest ngramsRepo =
      Set.foldl' (toFlowListScores_Level2 k' ngramsRepo flc_origin')
                 flc_dest
                 (view flc_cont flc_origin')


    toFlowListScores_Level2 :: KeepAllParents
                       -> Map Text NgramsRepoElement
                       -> FlowCont Text FlowListScores
                       -> FlowCont Text FlowListScores
                       -> Text
                       -> FlowCont Text FlowListScores
    toFlowListScores_Level2 k'' ngramsRepo flc_origin'' flc_dest' t =
      case Map.lookup t ngramsRepo of
        Nothing  -> over flc_cont (Set.insert t) flc_dest'
        Just nre -> over flc_scores
                  ( (Map.alter (addParent k'' nre (view flc_cont flc_origin'')) t)
                  . (Map.alter (addList $ _nre_list nre) t)
                  ) flc_dest'

------------------------------------------------------------------------
-- | Main addFunctions to groupResolution the FlowListScores
-- Use patch-map library here
-- diff, transformWith patches simplifies functions below
addList :: ListType
        -> Maybe FlowListScores
        -> Maybe FlowListScores
addList l Nothing =
  Just $ FlowListScores Map.empty (addListScore l Map.empty)

addList l (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent (addListScore l mapList)

-- * Unseful but nice comment:
-- "the addList function looks like an ASCII bird"

-- | Concrete function to pass to PatchMap
addListScore :: ListType -> Map ListType Int -> Map ListType Int
addListScore l m = Map.alter (plus l) l  m
  where
    plus CandidateTerm Nothing  = Just 1
    plus CandidateTerm (Just x) = Just $ x + 1

    plus MapTerm Nothing        = Just 2
    plus MapTerm (Just x)       = Just $ x + 2

    plus StopTerm Nothing       = Just 3
    plus StopTerm (Just x)      = Just $ x + 3

------------------------------------------------------------------------
data KeepAllParents = KeepAllParents Bool

addParent :: KeepAllParents -> NgramsRepoElement -> Set Text
          -> Maybe FlowListScores
          -> Maybe FlowListScores

addParent k nre ss Nothing  =
  Just $ FlowListScores mapParent Map.empty
    where
      mapParent = addParentScore k (_nre_parent nre) ss Map.empty

addParent k nre ss (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent' mapList
    where
      mapParent' = addParentScore k (_nre_parent nre) ss mapParent

addParentScore :: Num a
           => KeepAllParents
           -> Maybe NgramsTerm
           -> Set Text
           -> Map Text a
           -> Map Text a
addParentScore _ Nothing               _ss mapParent = mapParent
addParentScore (KeepAllParents k) (Just (NgramsTerm p')) ss mapParent =
  case k of
    True  -> Map.alter addCount p' mapParent
    False -> case Set.member p' ss of
               False -> mapParent
               True  -> Map.alter addCount p' mapParent
  where
        addCount Nothing  = Just 1
        addCount (Just n) = Just $ n + 1

------------------------------------------------------------------------
------------------------------------------------------------------------
