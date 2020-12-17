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
import Data.HashMap.Strict (HashMap)
import Data.Monoid (mempty)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Types.Main
import Gargantext.Prelude
import qualified Gargantext.Data.HashMap.Strict.Utils as HashMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set   as Set

------------------------------------------------------------------------
-- | Generates Score from list of HashMap Text NgramsRepoElement
toFlowListScores :: KeepAllParents
                 ->  FlowCont NgramsTerm FlowListScores
                 -> [HashMap NgramsTerm NgramsRepoElement]
                 ->  FlowCont NgramsTerm FlowListScores
toFlowListScores k flc_origin = foldl' (toFlowListScores_Level1 k flc_origin) mempty

  where
    toFlowListScores_Level1 :: KeepAllParents
                            -> FlowCont NgramsTerm FlowListScores
                            -> FlowCont NgramsTerm FlowListScores
                            -> HashMap NgramsTerm NgramsRepoElement
                            -> FlowCont NgramsTerm FlowListScores
    toFlowListScores_Level1 k' flc_origin' flc_dest ngramsRepo =
      Set.foldl' (toFlowListScores_Level2 k' ngramsRepo   flc_origin')
                 flc_dest
                 (Set.fromList $ HashMap.keys $ view flc_cont flc_origin')

    toFlowListScores_Level2 :: KeepAllParents
                            -> HashMap NgramsTerm NgramsRepoElement
                            -> FlowCont NgramsTerm FlowListScores
                            -> FlowCont NgramsTerm FlowListScores
                            -> NgramsTerm
                            -> FlowCont NgramsTerm FlowListScores
    toFlowListScores_Level2 k'' ngramsRepo flc_origin'' flc_dest' t =
      case HashMap.lookup t ngramsRepo of
        Nothing  -> over flc_cont   (HashMap.union $ HashMap.singleton t mempty) flc_dest'
        Just nre -> updateScoresParent k'' ngramsRepo nre flc_origin''
                  $ updateScores       k'' t nre setText flc_dest'
          where
            setText = Set.fromList
                    $ HashMap.keys
                    $ view flc_cont flc_origin''


    updateScoresParent :: KeepAllParents -> HashMap NgramsTerm NgramsRepoElement -> NgramsRepoElement
                       -> FlowCont NgramsTerm FlowListScores
                       -> FlowCont NgramsTerm FlowListScores
                       -> FlowCont NgramsTerm FlowListScores
    updateScoresParent keep@(KeepAllParents k''') ngramsRepo nre flc_origin'' flc_dest'' = case k''' of
                  False -> flc_dest''
                  True  -> case view nre_parent nre of
                    Nothing     -> flc_dest''
                    Just parent -> toFlowListScores_Level2 keep ngramsRepo flc_origin'' flc_dest'' parent


------------------------------------------------------------------------
updateScores :: KeepAllParents
             -> NgramsTerm -> NgramsRepoElement -> Set NgramsTerm
             -> FlowCont NgramsTerm FlowListScores
             -> FlowCont NgramsTerm FlowListScores
updateScores k t nre setText mtf =
    over flc_cont   ( HashMap.delete t)
  $ over flc_scores ((HashMap.alter (addParent k nre setText  ) t)
                    .(HashMap.alter (addList $ view nre_list nre) t)
                    ) mtf

------------------------------------------------------------------------
-- | Main addFunctions to groupResolution the FlowListScores
-- Use patch-map library here
addList :: ListType
        -> Maybe FlowListScores
        -> Maybe FlowListScores
addList l Nothing =
  Just $ set fls_listType (addListScore l mempty) mempty

addList l (Just fls) =
  Just $ over fls_listType (addListScore l) fls

-- * Unseful but nice comment:
-- "the addList function looks like an ASCII bird"

-- | Concrete function to pass to PatchMap
addListScore :: ListType -> HashMap ListType Int -> HashMap ListType Int
addListScore l m = HashMap.alter (plus l) l  m
  where
    plus CandidateTerm Nothing  = Just 1
    plus CandidateTerm (Just x) = Just $ x + 1

    plus MapTerm Nothing        = Just 1
    plus MapTerm (Just x)       = Just $ x + 1

    plus StopTerm Nothing       = Just 1
    plus StopTerm (Just x)      = Just $ x + 1

------------------------------------------------------------------------
data KeepAllParents = KeepAllParents Bool

addParent :: KeepAllParents -> NgramsRepoElement -> Set NgramsTerm
          -> Maybe FlowListScores
          -> Maybe FlowListScores

addParent k nre ss Nothing  =
  Just $ FlowListScores mempty mapParent
    where
      mapParent = addParentScore k (view nre_parent nre) ss mempty

addParent k nre ss (Just fls{-(FlowListScores mapList mapParent)-}) =
  Just $ over fls_parents (addParentScore k (view nre_parent nre) ss) fls

addParentScore :: Num a
           => KeepAllParents
           -> Maybe NgramsTerm
           -> Set NgramsTerm
           -> HashMap NgramsTerm a
           -> HashMap NgramsTerm a
addParentScore _ Nothing                                   _ss mapParent = mapParent
addParentScore (KeepAllParents keep) (Just p') ss mapParent =
  case keep of
    True  -> HashMap.alter addCount p' mapParent
    False -> case Set.member p' ss of
               False -> mapParent
               True  -> HashMap.alter addCount p' mapParent
  where
        addCount Nothing  = Just 1
        addCount (Just n) = Just $ n + 1

------------------------------------------------------------------------
------------------------------------------------------------------------
