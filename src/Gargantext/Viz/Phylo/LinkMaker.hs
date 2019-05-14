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
import Data.List                    ((++), sortOn, null, tail, splitAt, elem, concat, sort, delete, intersect)
import Data.Tuple.Extra
import Data.Map                     (Map,(!))
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.Metrics.Proximity
import Gargantext.Viz.Phylo.Aggregates.Cooc
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import qualified Data.Vector.Storable as VS
import Debug.Trace (trace)
import Numeric.Statistics (percentile)


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


-- | To set the LevelLink of all the PhyloGroups of a Phylo
setLevelLinks :: (Level,Level) -> Phylo -> Phylo
setLevelLinks (lvl,lvl') p = alterPhyloGroups (\gs -> map (\g -> if getGroupLevel g == lvl
                                                                  then linkGroupToGroups (lvl,lvl') g (filterCandidates g 
                                                                                                       $ filter (\g' -> getGroupPeriod g' == getGroupPeriod g) gs')
                                                                  else g) gs) p
  where
    --------------------------------------
    gs' :: [PhyloGroup]
    gs' = getGroupsWithLevel lvl' p
    --------------------------------------


------------------------------------------------------------------------
-- | Make links from Period to Period


-- | To apply the corresponding proximity function based on a given Proximity
applyProximity :: Proximity -> PhyloGroup -> PhyloGroup -> Map (Int, Int) Double -> (PhyloGroupId, Double)
applyProximity prox g1 g2 cooc = case prox of
  WeightedLogJaccard (WLJParams _ s) -> ((getGroupId g2), weightedLogJaccard s (getSubCooc (getGroupNgrams g1) cooc) (getSubCooc (getGroupNgrams g2) cooc))
  Hamming (HammingParams _)          -> ((getGroupId g2), hamming (getSubCooc (getGroupNgrams g1) cooc) (getSubCooc (getGroupNgrams g2) cooc))
  _                                  -> panic ("[ERR][Viz.Phylo.Example.applyProximity] Proximity function not defined")


-- | To get the next or previous PhyloPeriod based on a given PhyloPeriodId
getNextPeriods :: Filiation -> PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
getNextPeriods to' id l = case to' of
    Descendant -> (tail . snd) next
    Ascendant  -> (reverse . fst) next
    _          -> panic ("[ERR][Viz.Phylo.Example.getNextPeriods] Filiation type not defined")
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


-- | To find the best candidates regarding a given proximity
findBestCandidates' :: Filiation -> Int -> Int -> Proximity -> [PhyloPeriodId] -> [PhyloGroup] -> PhyloGroup -> Phylo -> ([Pointer],[Double])
findBestCandidates' fil depth limit prox prds gs g p
  | depth > limit || null next = ([],[])
  | (not . null) bestScores    = (take 2 bestScores, map snd scores)
  | otherwise                  = findBestCandidates' fil (depth + 1) limit prox prds gs g p
  where
    --------------------------------------
    next :: [PhyloPeriodId]
    next = take depth prds
    --------------------------------------
    cooc :: Map (Int, Int) Double
    cooc = getCooc next p
    --------------------------------------
    candidates :: [PhyloGroup]
    candidates = filter (\g' -> elem (getGroupPeriod g') next) gs 
    --------------------------------------
    scores :: [(PhyloGroupId, Double)]
    scores = map (\g' -> applyProximity prox g g' cooc) candidates
    --------------------------------------       
    bestScores :: [(PhyloGroupId, Double)]
    bestScores = reverse
               $ sortOn snd
               $ filter (\(_id,score) -> case prox of
                  WeightedLogJaccard (WLJParams thr _) -> score >= thr
                  Hamming (HammingParams thr)          -> score <= thr
                  Filiation                            -> panic "[ERR][Viz.Phylo.LinkMaker.findBestCandidates] Filiation"
                  ) scores
    --------------------------------------               


-- | To add some Pointer to a PhyloGroup
addPointers' :: Filiation -> [Pointer] -> PhyloGroup -> PhyloGroup
addPointers' fil pts g = g & case fil of 
   Descendant -> phylo_groupPeriodChilds  %~ (++ pts)
   Ascendant  -> phylo_groupPeriodParents %~ (++ pts)
   _          -> panic ("[ERR][Viz.Phylo.LinkMaker.addPointers] Wrong type of filiation")



-- | To update a list of pkyloGroups with some Pointers
updateGroups :: Filiation -> Level -> Map PhyloGroupId [Pointer] -> Phylo -> Phylo 
updateGroups fil lvl m p = alterPhyloGroups (\gs -> map (\g -> if (getGroupLevel g) == lvl
                                                               then addPointers' fil (m ! (getGroupId g)) g
                                                               else g ) gs) p



-- | Optimisation : to keep only the groups that have at least one ngrams in commons with the target
filterCandidates :: PhyloGroup -> [PhyloGroup] -> [PhyloGroup]
filterCandidates g gs = filter (\g' -> (not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g'))
                      $ delete g gs  



-- | To apply the intertemporal matching to Phylo at a given level
interTempoMatching :: Filiation -> Level -> Proximity -> Phylo -> Phylo
interTempoMatching fil lvl prox p = traceMatching fil lvl (getThreshold prox) scores
                                  $ updateGroups fil lvl pointers p
  where
    --------------------------------------
    pointers :: Map PhyloGroupId [Pointer]
    pointers = Map.fromList $ map (\(id,x) -> (id,fst x)) candidates
    --------------------------------------
    scores :: [Double]
    scores = sort $ concat $ map (snd . snd) candidates 
    --------------------------------------     
    candidates :: [(PhyloGroupId,([Pointer],[Double]))]
    candidates = map (\g -> ( getGroupId g, findBestCandidates' fil 1 5 prox (getNextPeriods fil (getGroupPeriod g) prds) (filterCandidates g gs) g p)) gs
    --------------------------------------
    gs :: [PhyloGroup]
    gs = getGroupsWithLevel lvl p
    --------------------------------------
    prds :: [PhyloPeriodId]
    prds = getPhyloPeriods p
    --------------------------------------


----------------
-- | Tracer | --
----------------


traceMatching :: Filiation -> Level -> Double -> [Double] -> Phylo -> Phylo
traceMatching fil lvl thr lst p = trace ( "----\n" <> show (fil) <> " unfiltered temporal Matching in Phylo" <> show (lvl) <> " :\n"
                                      <> "count : " <> show (length lst) <> " potential pointers (" <> show (length $ filter (>= thr) lst) <> " >= " <> show (thr) <> ")\n"
                                      <> "similarity : " <> show (percentile 25 (VS.fromList lst)) <> " (25%) "
                                                         <> show (percentile 50 (VS.fromList lst)) <> " (50%) "
                                                         <> show (percentile 75 (VS.fromList lst)) <> " (75%) "
                                                         <> show (percentile 90 (VS.fromList lst)) <> " (90%)\n") p

