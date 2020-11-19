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
                 ->  FlowListCont Text
                 -> [Map Text NgramsRepoElement]
                 ->  FlowListCont Text
toFlowListScores k flc = foldl' (toFlowListScores' k flc) mempty
  where
    toFlowListScores' :: KeepAllParents
                     -> FlowListCont Text
                     -> FlowListCont Text
                     -> Map Text NgramsRepoElement
                     -> FlowListCont Text
    toFlowListScores' k' flc flc' ngramsRepo =
      Set.foldl' (toFlowListScores'' k' ngramsRepo flc) flc' (view flc_cont flc)


    toFlowListScores'' :: KeepAllParents
                       -> Map Text NgramsRepoElement
                       -> FlowListCont Text
                       -> FlowListCont Text
                       -> Text
                       -> FlowListCont Text
    toFlowListScores'' k'' ngramsRepo flc to'' t =
      case Map.lookup t ngramsRepo of
        Nothing  -> over flc_cont (Set.insert t) to''
        Just nre -> over flc_scores
                  ( (Map.alter (addParent k'' nre (view flc_cont flc)) t)
                  . (Map.alter (addList $ _nre_list nre) t)
                  ) to''

------------------------------------------------------------------------
-- | Main addFunctions to groupResolution the FlowListScores
-- Use patch-map library here
-- diff, transformWith patches simplifies functions below
addList :: ListType
        -> Maybe FlowListScores
        -> Maybe FlowListScores
addList l Nothing =
  Just $ FlowListScores Map.empty (addList' l Map.empty)

addList l (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent mapList'
    where
      mapList' = addList' l mapList
-- * Unseful but nice comment:
-- "the addList function looks like an ASCII bird"

-- | Concrete function to pass to PatchMap
addList' :: ListType -> Map ListType Int -> Map ListType Int
addList' l m = Map.alter (plus l) l  m
  where
    plus CandidateTerm Nothing  = Just 1
    plus CandidateTerm (Just x) = Just $ x + 1

    plus MapTerm Nothing        = Just 2
    plus MapTerm (Just x)       = Just $ x + 2

    plus StopTerm Nothing       = Just 3
    plus StopTerm (Just x)      = Just $ x + 3

------------------------------------------------------------------------
------------------------------------------------------------------------
data KeepAllParents = KeepAllParents Bool

addParent :: KeepAllParents -> NgramsRepoElement -> Set Text
          -> Maybe FlowListScores
          -> Maybe FlowListScores

addParent k nre ss Nothing  =
  Just $ FlowListScores mapParent Map.empty
    where
      mapParent = addParent' k (_nre_parent nre) ss Map.empty

addParent k nre ss (Just (FlowListScores mapParent mapList)) =
  Just $ FlowListScores mapParent' mapList
    where
      mapParent' = addParent' k (_nre_parent nre) ss mapParent

addParent' :: Num a
           => KeepAllParents
           -> Maybe NgramsTerm
           -> Set Text
           -> Map Text a
           -> Map Text a
addParent' _ Nothing               _ss mapParent = mapParent
addParent' (KeepAllParents k) (Just (NgramsTerm p')) ss mapParent =
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
