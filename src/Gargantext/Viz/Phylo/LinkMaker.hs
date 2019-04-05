{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Viz.Phylo.LinkMaker
  where

import Control.Lens                 hiding (both, Level)
import Data.List                    ((++), nub, sortOn, head, null, tail, splitAt, (!!), elem)
import Data.Tuple.Extra

import Gargantext.Prelude           hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.Metrics.Proximity

import qualified Data.List  as List
import qualified Data.Maybe as Maybe


------------------------------------------------------------------------
-- | Make links from Level to Level


-- | To choose a LevelLink strategy based an a given Level
shouldLink :: (Level,Level) -> PhyloGroup -> PhyloGroup -> Bool
shouldLink (lvl,_lvl) g g'
  | lvl <= 1  = doesContainsOrd (getGroupNgrams g) (getGroupNgrams g')
  | lvl >  1  = elem (getGroupId g) (getGroupLevelChildsId g')
  | otherwise = panic ("[ERR][Viz.Phylo.LinkMaker.shouldLink] Level not defined")


-- | To set the LevelLinks between a given PhyloGroup and a list of childs/parents PhyloGroups
linkGroupToGroups :: (Level,Level) -> PhyloGroup -> [PhyloGroup] -> PhyloGroup
linkGroupToGroups (lvl,lvl') current targets
  | lvl < lvl' = setLevelParents current
  | lvl > lvl' = setLevelChilds current
  | otherwise = current
  where
    --------------------------------------
    setLevelChilds :: PhyloGroup -> PhyloGroup
    setLevelChilds =  over (phylo_groupLevelChilds) addPointers
    --------------------------------------
    setLevelParents :: PhyloGroup -> PhyloGroup
    setLevelParents =  over (phylo_groupLevelParents) addPointers
    --------------------------------------
    addPointers :: [Pointer] -> [Pointer]
    addPointers lp = lp ++ Maybe.mapMaybe (\target ->
                                            if shouldLink (lvl,lvl') current target
                                            then Just ((getGroupId target),1)
                                            else Nothing) targets
    --------------------------------------


-- | To set the LevelLinks between two lists of PhyloGroups
linkGroupsByLevel :: (Level,Level) -> Phylo -> [PhyloGroup] -> [PhyloGroup]
linkGroupsByLevel (lvl,lvl') p groups  = map (\group ->
                                              if getGroupLevel group == lvl
                                              then linkGroupToGroups (lvl,lvl') group (getGroupsWithFilters lvl' (getGroupPeriod group) p)
                                              else group) groups


-- | To set the LevelLink of all the PhyloGroups of a Phylo
setLevelLinks :: (Level,Level) -> Phylo -> Phylo
setLevelLinks (lvl,lvl') p = alterPhyloGroups (linkGroupsByLevel (lvl,lvl') p) p


------------------------------------------------------------------------
-- | Make links from Period to Period


-- | To apply the corresponding proximity function based on a given Proximity
getProximity :: (Proximity,[Double]) -> PhyloGroup -> PhyloGroup -> (PhyloGroupId, Double)
getProximity (prox,param) g1 g2 = case prox of
  WeightedLogJaccard -> ((getGroupId g2),weightedLogJaccard (param !! 1) (getGroupCooc g1) (unifySharedKeys (getGroupCooc g2) (getGroupCooc g1)))
  Hamming            -> ((getGroupId g2),hamming (getGroupCooc g1) (unifySharedKeys (getGroupCooc g2) (getGroupCooc g1)))
  _                  -> panic ("[ERR][Viz.Phylo.Example.getProximity] Proximity function not defined")


-- | To get the next or previous PhyloPeriod based on a given PhyloPeriodId
getNextPeriods :: PairTo -> PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
getNextPeriods to' id l = case to' of
    Childs  -> unNested id ((tail . snd) next)
    Parents -> unNested id ((reverse . fst) next)
    --_       -> panic ("[ERR][Viz.Phylo.Example.getNextPeriods] PairTo type not defined")
    where
      --------------------------------------
      next :: ([PhyloPeriodId], [PhyloPeriodId])
      next = splitAt idx l
      --------------------------------------
      idx :: Int
      idx = case (List.elemIndex id l) of
        Nothing -> panic ("[ERR][Viz.Phylo.Example.getNextPeriods] PhyloPeriodId not defined")
        Just i  -> i
      --------------------------------------
      -- | To have an non-overlapping next period
      unNested :: PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
      unNested x l'
        | null l'                  = []
        | nested (fst $ head l') x = unNested x (tail l')
        | nested (snd $ head l') x = unNested x (tail l')
        | otherwise                = l'
      --------------------------------------
      nested :: Date -> PhyloPeriodId -> Bool
      nested d prd = d >= fst prd && d <= snd prd
      --------------------------------------


-- | To find the best set (max = 2) of Childs/Parents candidates based on a given Proximity mesure until a maximum depth (max = Period + 5 units )
findBestCandidates :: PairTo -> Int -> Int -> (Proximity,[Double]) -> PhyloGroup -> Phylo -> [(PhyloGroupId, Double)]
findBestCandidates to' depth max' (prox,param) group p
  | depth > max' || null next = []
  | (not . null) best = take 2 best
  | otherwise = findBestCandidates to' (depth + 1) max' (prox,param) group p
  where
    --------------------------------------
    next :: [PhyloPeriodId]
    next = getNextPeriods to' (getGroupPeriod group) (getPhyloPeriods p)
    --------------------------------------
    candidates :: [PhyloGroup]
    candidates = getGroupsWithFilters (getGroupLevel group) (head next) p
    --------------------------------------
    scores :: [(PhyloGroupId, Double)]
    scores = map (\group' -> getProximity (prox,param) group group') candidates
    --------------------------------------
    best :: [(PhyloGroupId, Double)]
    best = reverse
         $ sortOn snd
         $ filter (\(_id,score) -> case prox of
                    WeightedLogJaccard -> score >= (param !! 0)
                    Hamming            -> score <= (param !! 0)
                    _                  -> panic "[ERR][Viz.Phylo.LinkMaker.findBestCandidates] Not implemented yet"
                  ) scores
    --------------------------------------


-- | To add a new list of Pointers into an existing Childs/Parents list of Pointers
makePair :: PairTo -> PhyloGroup -> [(PhyloGroupId, Double)] -> PhyloGroup
makePair to' group ids = case to' of
    Childs    -> over (phylo_groupPeriodChilds) addPointers group
    Parents   -> over (phylo_groupPeriodParents) addPointers group
    --_        -> panic ("[ERR][Viz.Phylo.Example.makePair] PairTo type not defined")
    where
      --------------------------------------
      addPointers :: [Pointer] -> [Pointer]
      addPointers l = nub $ (l ++ ids)
      --------------------------------------


-- | To pair all the Phylogroups of given PhyloLevel to their best Parents or Childs
interTempoMatching :: PairTo -> Level -> (Proximity,[Double]) -> Phylo -> Phylo
interTempoMatching to' lvl (prox,param) p = alterPhyloGroups
                                    (\groups ->
                                      map (\group ->
                                            if (getGroupLevel group) == lvl
                                            then
                                              let
                                                --------------------------------------
                                                candidates :: [(PhyloGroupId, Double)]
                                                candidates = findBestCandidates to' 1 5 (prox,param) group p
                                                --------------------------------------
                                              in
                                                makePair to' group candidates
                                            else
                                              group ) groups) p
