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
import Data.List                    ((++), sortOn, null, tail, splitAt, elem, concat, sort, delete, intersect, nub, groupBy)
import Data.Tuple.Extra
import Data.Map                     (Map,(!),fromListWith,elems,restrictKeys,unionWith)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.Metrics.Proximity
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import qualified Data.Vector.Storable as VS
import Debug.Trace (trace)
import Numeric.Statistics (percentile)

-----------------------------
-- | From Level to level | --
-----------------------------


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


-------------------------------
-- | From Period to Period | --
-------------------------------


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


-- | To get the number of docs produced during a list of periods
periodsToNbDocs :: [PhyloPeriodId] -> Phylo -> Double
periodsToNbDocs prds phylo = sum $ elems 
                           $ restrictKeys (phylo ^. phylo_docsByYears)
                           $ periodsToYears prds 


-- | To process a given Proximity
processProximity :: Proximity -> Map (Int, Int) Double -> Map (Int, Int) Double -> Double -> Double
processProximity proximity cooc cooc' nbDocs = case proximity of 
  WeightedLogJaccard (WLJParams _ sens) -> weightedLogJaccard sens cooc cooc' nbDocs
  Hamming (HammingParams _)             -> hamming cooc cooc'
  _                                     -> panic "[ERR][Viz.Phylo.LinkMaker.processProximity] Unknown proximity"



-- | Find the best candidates to be time-linked with a group g1 (recursively until the limit of periods is reached)
-- | 1) find the next periods and get the mini cooc matrix of g1
-- | 2) build the pairs of candidates (single groups or tuples)
-- | 3) process the proximity mesure and select the best ones to create the pointers (ie: all the max)
findBestCandidates :: Filiation -> Int -> Int -> Proximity -> [(Date,Date)] -> [PhyloGroup] -> PhyloGroup -> Phylo -> ([Pointer],[Double])
findBestCandidates filiation depth limit proximity periods candidates g1 phylo
  | depth > limit || null nextPeriods = ([],[])
  | (not . null) pointers             = (head' "findBestCandidates" $ groupBy (\x y -> snd x == snd y) pointers
                                        ,map snd similarities)
  | otherwise                         = findBestCandidates filiation (depth + 1) limit proximity periods candidates g1 phylo
  where  
    --------------------------------------
    pointers :: [(PhyloGroupId, Double)]
    pointers = reverse $ sortOn snd $ filter (\(_,score) -> case proximity of
                  WeightedLogJaccard (WLJParams thr _)   -> score >= thr
                  Hamming (HammingParams thr)            -> score <= thr
                  _                                      -> panic "[ERR][Viz.Phylo.LinkMaker.findBestCandidates] Unknown proximity"
                  ) similarities
    --------------------------------------
    similarities :: [(PhyloGroupId, Double)]
    similarities = concat $ map (\(g2,g3) -> let nbDocs = periodsToNbDocs [(getGroupPeriod g1),(getGroupPeriod g2),(getGroupPeriod g3)] phylo
                                                 cooc2  = getGroupCooc g2
                                                 cooc3  = getGroupCooc g3
                                                 score  = processProximity proximity cooc1 (unionWith (+) cooc2 cooc3) nbDocs 
                                             in  nub $ [(getGroupId g2,score),(getGroupId g3,score)]) pairsOfCandidates
    --------------------------------------
    pairsOfCandidates :: [(PhyloGroup,PhyloGroup)]
    pairsOfCandidates = listToFullCombi $ filter (\g -> elem (getGroupPeriod g) nextPeriods) candidates
    --------------------------------------
    cooc1 :: Map (Int,Int) Double
    cooc1 = getGroupCooc g1
    --------------------------------------
    nextPeriods :: [(Date,Date)]
    nextPeriods = take depth periods
    --------------------------------------             


-- | To add some Pointer to a PhyloGroup
addPointers' :: Filiation -> [Pointer] -> PhyloGroup -> PhyloGroup
addPointers' fil pts g = g & case fil of 
   Descendant -> phylo_groupPeriodChilds  %~ (++ pts)
   Ascendant  -> phylo_groupPeriodParents %~ (++ pts)
   _          -> panic ("[ERR][Viz.Phylo.LinkMaker.addPointers] Wrong type of filiation")



-- | To update a list of phyloGroups with some Pointers
updateGroups :: Filiation -> Level -> Map PhyloGroupId [Pointer] -> Phylo -> Phylo 
updateGroups fil lvl m p = alterPhyloGroups (\gs -> map (\g -> if (getGroupLevel g) == lvl
                                                               then addPointers' fil (m ! (getGroupId g)) g
                                                               else g ) gs) p



-- | Optimisation : to keep only the groups that have at least one ngrams in commons with the target
filterCandidates :: PhyloGroup -> [PhyloGroup] -> [PhyloGroup]
filterCandidates g gs = filter (\g' -> (not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g'))
                      $ delete g gs


-- | a init avec la [[head groups]] et la tail groups
toBranches :: [[PhyloGroup]] -> [PhyloGroup] -> [[PhyloGroup]]
toBranches mem gs
  | null gs = mem
  | otherwise = toBranches mem' $ tail gs
  where
    --------------------------------------
    mem' :: [[PhyloGroup]]
    mem' = if (null withHead)
           then mem ++ [[head' "toBranches" gs]]
           else (filter (\gs' -> not $ elem gs' withHead) mem)
                ++
                [(concat withHead) ++ [head' "toBranches" gs]]
    --------------------------------------
    withHead :: [[PhyloGroup]]
    withHead = filter (\gs' -> (not . null)
                             $ intersect (concat $ map getGroupNgrams gs')
                                         (getGroupNgrams $ (head' "toBranches" gs))
                      ) mem
    --------------------------------------


-- | To process an intertemporal matching task to a Phylo at a given level
-- | 1) split all groups (of the level) in branches (ie:related components sharing at least one ngram)
-- | 2) for each branch, for each group find the best candidates (by Filiation and Proximity) and create the corresponding pointers
-- | 3) update all the groups with the new pointers if they exist
interTempoMatching :: Filiation -> Level -> Proximity -> Phylo -> Phylo
interTempoMatching fil lvl prox p = traceMatching fil lvl (getThreshold prox) debug $ updateGroups fil lvl pointersMap p
  where
    --------------------------------------
    debug :: [Double]
    debug = sort $ concat $ map (snd . snd) pointers     
    --------------------------------------
    pointersMap :: Map PhyloGroupId [Pointer]
    pointersMap = Map.fromList $ map (\(id,x) -> (id,fst x)) pointers
    --------------------------------------
    pointers :: [(PhyloGroupId,([Pointer],[Double]))]
    pointers = concat 
                 $ map (\branche -> 
                    map (\g -> ( getGroupId g
                               , findBestCandidates fil 1 5 prox (getNextPeriods fil (getGroupPeriod g) (getPhyloPeriods p)) (filterCandidates g branche) g p )
                         ) branche ) branches
    --------------------------------------
    branches :: [[PhyloGroup]]
    branches = tracePreBranches
             $ toBranches [[head' "interTempoMatching" (getGroupsWithLevel lvl p)]] 
             $ tail (getGroupsWithLevel lvl p)
    --------------------------------------


------------------------------------------------------------------------
-- | Make links from Period to Period after level 1

toLevelUp :: [Pointer] -> Phylo -> [Pointer]
toLevelUp lst p = Map.toList 
                $ map (\ws -> maximum ws)
                $ fromListWith (++) [(id, [w]) | (id, w) <- pointers]
  where
    --------------------------------------
    pointers :: [Pointer]
    pointers = map (\(id,v) -> (getGroupLevelParentId $ getGroupFromId id p, v)) lst
    --------------------------------------


-- | Transpose the parent/child pointers from one level to another
transposePeriodLinks :: Level -> Phylo -> Phylo
transposePeriodLinks lvl p = alterGroupWithLevel
  (\g ->
    --------------------------------------
    let childs  = getGroupsFromIds (map fst $ getGroupLevelChilds g) p
        ascLink = toLevelUp (concat $ map getGroupPeriodParents childs) p 
        desLink = toLevelUp (concat $ map getGroupPeriodChilds  childs) p
    --------------------------------------
    in g & phylo_groupPeriodParents  %~ (++ ascLink)
         & phylo_groupPeriodChilds   %~ (++ desLink)
    --------------------------------------
  ) lvl p 


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

tracePreBranches :: [[PhyloGroup]] -> [[PhyloGroup]]
tracePreBranches bs = trace (show (length bs) <> " pre-branches" <> "\n"
                             <> "with sizes : " <> show (map length bs) <> "\n") bs

