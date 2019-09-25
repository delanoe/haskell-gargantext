{-|
Module      : Gargantext.Viz.Phylo.TemporalMatching
Description : Module dedicated to the adaptative temporal matching of a Phylo.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gargantext.Viz.Phylo.TemporalMatching where

import Data.List (concat, splitAt, tail, sortOn, (++), intersect, null, inits, groupBy, scanl, nub, union, elemIndex, (!!), dropWhile, partition)
import Data.Map  (Map, fromList, elems, restrictKeys, unionWith, intersectionWith, findWithDefault, filterWithKey)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools

import Prelude (logBase)
import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
-- import Debug.Trace (trace)

import qualified Data.Set as Set


-------------------
-- | Proximity | --
-------------------


-- | Process the inverse sumLog
sumInvLog :: Double -> [Double] -> Double
sumInvLog s l = foldl (\mem x -> mem + (1 / log (s + x))) 0 l


-- | Process the sumLog
sumLog :: Double -> [Double] -> Double
sumLog s l = foldl (\mem x -> mem + log (s + x)) 0 l  


-- | To compute a jaccard similarity between two lists
jaccard :: [Int] -> [Int] -> Double
jaccard inter' union' = ((fromIntegral . length) $ inter') / ((fromIntegral . length) $ union')


-- | To process a WeighedLogJaccard distance between to coocurency matrix
weightedLogJaccard :: Double -> Double -> Cooc -> Cooc -> [Int] -> [Int] -> Double
weightedLogJaccard sens docs cooc cooc' ngrams ngrams'
  | null ngramsInter           = 0
  | ngramsInter == ngramsUnion = 1
  | sens == 0    = jaccard ngramsInter ngramsUnion
  | sens > 0     = (sumInvLog sens coocInter) / (sumInvLog sens coocUnion)
  | otherwise    = (sumLog sens coocInter) / (sumLog sens coocUnion)
  where
    --------------------------------------
    ngramsInter :: [Int] 
    ngramsInter = intersect ngrams ngrams'   
    --------------------------------------
    ngramsUnion :: [Int] 
    ngramsUnion = union ngrams ngrams'
    --------------------------------------
    coocInter :: [Double]
    coocInter = elems $ map (/docs) $ filterWithKey (\(k,k') _ -> k == k') $ intersectionWith (+) cooc cooc'      
    --------------------------------------
    coocUnion :: [Double]
    coocUnion = elems $ map (/docs) $ filterWithKey (\(k,k') _ -> k == k') $ unionWith (+) cooc cooc'
    --------------------------------------


-- | To choose a proximity function
pickProximity :: Proximity -> Double -> Cooc -> Cooc -> [Int] -> [Int] -> Double
pickProximity proximity docs cooc cooc' ngrams ngrams' = case proximity of
    WeightedLogJaccard sens _ _ -> weightedLogJaccard sens docs cooc cooc' ngrams ngrams'
    Hamming -> undefined


-- | To process the proximity between a current group and a pair of targets group
toProximity :: Map Date Double -> Proximity -> PhyloGroup -> PhyloGroup -> PhyloGroup -> Double
toProximity docs proximity ego target target' = 
    let docs'  = sum $ elems docs
        cooc   = if target == target'
                 then (target ^. phylo_groupCooc)
                 else sumCooc (target ^. phylo_groupCooc) (target' ^. phylo_groupCooc)
        ngrams = if target == target'
                 then (target ^. phylo_groupNgrams)
                 else union (target ^. phylo_groupNgrams) (target' ^. phylo_groupNgrams)
    in pickProximity proximity docs' (ego ^. phylo_groupCooc) cooc (ego ^. phylo_groupNgrams) ngrams 


------------------------
-- | Local Matching | --
------------------------


-- | Find pairs of valuable candidates to be matched
makePairs :: [PhyloGroup] -> [PhyloPeriodId] -> [PhyloPeriodId] -> [(PhyloGroup,PhyloGroup)]
makePairs candidates periods periods' = case null periods of
    True  -> []
          -- | at least on of the pair candidates should be from the last added period
    False -> filter (\(cdt,cdt') -> 
                ((inLastPeriod cdt periods) || (inLastPeriod cdt' periods))
             && (not $ inOldPeriods cdt  periods')
             && (not $ inOldPeriods cdt' periods'))
           $ listToKeys candidates
    where 
        inLastPeriod :: PhyloGroup -> [PhyloPeriodId] -> Bool
        inLastPeriod g prds = (g ^. phylo_groupPeriod) == (last' "makePairs" prds)
        --------------------------------------
        inOldPeriods :: PhyloGroup -> [PhyloPeriodId] -> Bool
        inOldPeriods g prds = elem (g ^. phylo_groupPeriod) prds


keepOldOnes :: Filiation -> Proximity -> Double -> PhyloGroup -> Bool
keepOldOnes fil proxi thr ego = any (\(_,w) -> filterProximity proxi thr w)
                              $ getPeriodPointers fil ego 

filterPointers :: Proximity -> Double -> [Pointer] -> [Pointer]
filterPointers proxi thr pts = filter (\(_,w) -> filterProximity proxi thr w) pts


findLastPeriod :: Filiation -> [Pointer] -> PhyloPeriodId
findLastPeriod fil pts = case fil of 
    ToParents -> head' "findLastPeriod" $ sortOn fst $ map (fst . fst . fst) pts
    ToChilds  -> head' "findLastPeriod" $ reverse $ sortOn fst $ map (fst . fst . fst) pts



phyloGroupMatching :: [[PhyloGroup]] -> Filiation -> Proximity -> Map Date Double -> Double -> PhyloGroup -> PhyloGroup
phyloGroupMatching candidates fil proxi docs thr ego = 
    if keepOldOnes fil proxi thr ego
        -- | keep some of the old pointers 
        then addPointers ego fil TemporalPointer
           $ filterPointers proxi thr
           $ getPeriodPointers fil ego 
        else case null pointers of
            -- | let's find new pointers
            True  -> addPointers ego fil TemporalPointer []
            False -> addPointers ego fil TemporalPointer
                   $ head' "phyloGroupMatching"
                   -- | Keep only the best set of pointers grouped by proximity
                   $ groupBy (\pt pt' -> snd pt == snd pt')
                   $ reverse $ sortOn snd $ head' "pointers" pointers
                   -- | Find the first time frame where at leats one pointer satisfies the proximity threshold
    where
        --------------------------------------
        oldPeriods :: [PhyloPeriodId] -> [PhyloPeriodId]
        oldPeriods periods = 
            if (null $ getPeriodPointers fil ego) 
                then []
                else 
                    let period = findLastPeriod fil $ getPeriodPointers fil ego
                     in fst $ partition (\prd -> case fil of 
                                    ToChilds  -> prd <= period
                                    ToParents -> prd >= period ) periods 
        --------------------------------------
        pointers :: [[Pointer]]
        pointers = take 1
                 $ dropWhile (null)
                 -- | for each time frame, process the proximity on relevant pairs of targeted groups
                 $ scanl (\acc groups ->
                            let periods  = nub 
                                         $ concat $ map (\gs -> if null gs
                                                                then []
                                                                else [_phylo_groupPeriod $ head' "pointers" gs]) groups
                                periods' = oldPeriods periods
                                pairs = makePairs (concat groups) periods periods'
                            in  acc ++ ( filterPointers proxi thr
                                       $ concat
                                       $ map (\(c,c') ->
                                                -- | process the proximity between the current group and a pair of candidates 
                                                let proximity = toProximity (filterDocs docs ([ego ^. phylo_groupPeriod] ++ periods)) proxi ego c c'
                                                in if (c == c')
                                                   then [(getGroupId c,proximity)]
                                                   else [(getGroupId c,proximity),(getGroupId c',proximity)] ) pairs)
                         ) []
                 -- | groups from [[1900],[1900,1901],[1900,1901,1902],...]
                 $ inits candidates


filterDocs :: Map Date Double -> [PhyloPeriodId] -> Map Date Double
filterDocs d pds = restrictKeys d $ periodsToYears pds


-----------------------------
-- | Matching Processing | --
-----------------------------


getNextPeriods :: Filiation -> Int -> PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
getNextPeriods fil max' pId pIds = 
    case fil of 
        ToChilds  -> take max' $ (tail . snd) $ splitAt (elemIndex' pId pIds) pIds
        ToParents -> take max' $ (reverse . fst) $ splitAt (elemIndex' pId pIds) pIds


getCandidates :: Filiation -> PhyloGroup -> [[PhyloGroup]] -> [[PhyloGroup]]
getCandidates fil ego targets = 
    case fil of
        ToChilds  -> targets'
        ToParents -> reverse targets'
    where
        targets' :: [[PhyloGroup]]
        targets' = 
            map (\groups' -> 
                    filter (\g' -> (not . null) $ intersect (ego ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams)
                ) groups') targets


phyloBranchMatching :: Int -> [PhyloPeriodId] -> Proximity -> Double -> Map Date Double -> [PhyloGroup] -> [PhyloGroup]
phyloBranchMatching frame periods proximity thr docs branch = traceBranchMatching proximity thr
                                                            $ matchByPeriods ToParents
                                                            $ groupByField _phylo_groupPeriod
                                                            $ matchByPeriods ToChilds
                                                            $ groupByField _phylo_groupPeriod branch
    where
        --------------------------------------
        matchByPeriods :: Filiation -> Map PhyloPeriodId [PhyloGroup] -> [PhyloGroup]
        matchByPeriods fil branch' = foldl' (\acc prd ->
            let periods'   = getNextPeriods fil frame prd periods
                candidates = map (\prd' -> findWithDefault [] prd' branch') periods'
                docs' = filterDocs docs ([prd] ++ periods')
                egos  = map (\g -> phyloGroupMatching (getCandidates fil g candidates) fil proximity docs' thr g)
                      $ findWithDefault [] prd branch'
                egos' = egos `using` parList rdeepseq
             in acc ++ egos' ) [] periods


-----------------------
-- | Phylo Quality | --
-----------------------


termFreq :: Int -> [[PhyloGroup]] -> Double
termFreq term branches = (sum $ map (\g -> findWithDefault 0 (term,term) (g ^. phylo_groupCooc)) $ concat branches)
                       / (sum $ map (\g -> getTrace $ g ^. phylo_groupCooc) $ concat branches)


entropy :: [[PhyloGroup]] -> Double
entropy branches = 
    let terms = ngramsInBranches branches
    in  sum $ map (\term -> (1 / log (termFreq term branches))
                          / (sum $ map (\branch -> 1 / log (termFreq term [branch])) branches)
                          * (sum $ map (\branch ->
                                             let q = branchObs term (length $ concat branches) branch
                                             in if (q == 0)
                                                then 0
                                                else - q * logBase 2 q ) branches) ) terms
    where
        -- | Probability to observe a branch given a random term of the phylo
        branchObs :: Int -> Int -> [PhyloGroup] -> Double
        branchObs term total branch = (fromIntegral $ length $ filter (\g -> elem term $ g ^. phylo_groupNgrams) branch)
                                    / (fromIntegral total)  


homogeneity :: [[PhyloGroup]] -> Double
homogeneity branches =
    let nbGroups = length $ concat branches
    in  sum 
      $ map (\branch -> (if (length branch == nbGroups)
                         then 1
                         else (1 / log (branchCov branch nbGroups))
                            / (sum $ map (\branch' -> 1 / log (branchCov branch' nbGroups)) branches))
                            * (sum $ map (\term -> (termFreq term branches)
                                           / (sum $ map (\term' -> termFreq term' branches) $ ngramsInBranches [branch])
                                           * (fromIntegral $ sum $ ngramsInBranches [filter (\g -> elem term $ g ^. phylo_groupNgrams) branch])
                                           / (fromIntegral $ sum $ ngramsInBranches [branch])
                                    ) $ ngramsInBranches [branch]) ) branches
    where 
        branchCov :: [PhyloGroup] -> Int -> Double
        branchCov branch total = (fromIntegral $ length branch) / (fromIntegral total) 


toPhyloQuality :: [[PhyloGroup]] -> Double
toPhyloQuality branches = sqrt (homogeneity branches / entropy branches)


-----------------------------
-- | Adaptative Matching | --
-----------------------------


groupsToBranches :: Map PhyloGroupId PhyloGroup -> [[PhyloGroup]]
groupsToBranches groups =
    -- | run the related component algorithm
    let graph = zip [1..]
              $ relatedComponents
              $ map (\group -> [getGroupId group] 
                            ++ (map fst $ group ^. phylo_groupPeriodParents)
                            ++ (map fst $ group ^. phylo_groupPeriodChilds) ) $ elems groups
    -- | update each group's branch id
    in map (\(bId,ids) ->
                map (\group -> group & phylo_groupBranchId %~ (\(lvl,lst) -> (lvl,lst ++ [bId])))
                $ elems $ restrictKeys groups (Set.fromList ids)) graph


recursiveMatching :: Proximity -> Double -> Int -> [PhyloPeriodId] -> Map Date Double -> Double -> [[PhyloGroup]] -> [PhyloGroup]
recursiveMatching proximity thr frame periods docs quality branches =
    if (length branches == (length $ concat branches))
        then concat branches
    else if thr > 1
        then concat branches
    else  
        case quality <= (sum nextQualities) of
                    -- | success : the new threshold improves the quality score, let's go deeper (traceMatchSuccess thr quality (sum nextQualities))
            True  -> concat
                   $ map (\branches' ->
                            let idx = fromJust $ elemIndex branches' nextBranches 
                            in  recursiveMatching proximity (thr + (getThresholdStep proximity)) frame periods docs (nextQualities !! idx) branches')
                   $ nextBranches
                    -- | failure : last step was a local maximum of quality, let's validate it (traceMatchFailure thr quality (sum nextQualities))
            False -> concat branches
    where
        -- | 2) for each of the possible next branches process the phyloQuality score
        nextQualities :: [Double]
        nextQualities = map toPhyloQuality nextBranches
        -- | 1) for each local branch process a temporal matching then find the resulting branches
        nextBranches :: [[[PhyloGroup]]]
        nextBranches = 
            let branches' = map (\branch -> phyloBranchMatching frame periods proximity thr docs branch) branches
                clusters  = map (\branch -> groupsToBranches $ fromList $ map (\group -> (getGroupId group, group)) branch) branches'
                clusters' = clusters `using` parList rdeepseq
             in clusters'



temporalMatching :: Phylo -> Phylo
temporalMatching phylo = updatePhyloGroups 1 branches' phylo
    where
        -- | 4) run the recursive matching to find the best repartition among branches
        branches' :: Map PhyloGroupId PhyloGroup
        branches' = fromList 
                 $ map (\g -> (getGroupId g, g))
                 $ traceMatchEnd
                 $ recursiveMatching (phyloProximity $ getConfig phylo) 
                                     ( (getThresholdInit $ phyloProximity $ getConfig phylo) 
                                     + (getThresholdStep $ phyloProximity $ getConfig phylo)) 
                                     (getTimeFrame $ timeUnit $ getConfig phylo)
                                     (getPeriodIds phylo)
                                     (phylo ^. phylo_timeDocs) quality branches
        -- | 3) process the quality score
        quality :: Double
        quality = toPhyloQuality branches
        -- | 2) group into branches
        branches :: [[PhyloGroup]] 
        branches = groupsToBranches $ fromList $ map (\group -> (getGroupId group, group)) groups'
        -- | 1) for each group process an initial temporal Matching
        groups' :: [PhyloGroup]
        groups' = phyloBranchMatching (getTimeFrame $ timeUnit $ getConfig phylo) (getPeriodIds phylo) 
                                  (phyloProximity $ getConfig phylo) (getThresholdInit $ phyloProximity $ getConfig phylo)
                                  (phylo ^. phylo_timeDocs) 
                                  (traceTemporalMatching $ getGroupsFromLevel 1 phylo)