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

import Data.List (concat, splitAt, tail, sortOn, (++), intersect, null, inits, find, groupBy, scanl, nub, union, elemIndex, (!!))
import Data.Map  (Map, fromList, fromListWith, filterWithKey, elems, restrictKeys, unionWith, intersectionWith, findWithDefault)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools

import Debug.Trace (trace)
import Prelude (logBase)
import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)

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
    coocInter = elems $ map (/docs) $ intersectionWith (+) cooc cooc'      
    --------------------------------------
    coocUnion :: [Double]
    coocUnion = elems $ map (/docs) $ unionWith (+) cooc cooc'
    --------------------------------------


-- | To choose a proximity function
pickProximity :: Proximity -> Double -> Cooc -> Cooc -> [Int] -> [Int] -> Double
pickProximity proximity docs cooc cooc' ngrams ngrams' = case proximity of
    WeightedLogJaccard sens _ _ -> weightedLogJaccard sens docs cooc cooc' ngrams ngrams'
    Hamming -> undefined


filterProximity :: Proximity -> Double -> Double -> Bool
filterProximity proximity thr local = 
    case proximity of
        WeightedLogJaccard _ _ _ -> local >= thr
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
makePairs :: [PhyloGroup] -> [PhyloPeriodId] -> [(PhyloGroup,PhyloGroup)]
makePairs candidates periods = case null periods of
    True  -> []
          -- | at least on of the pair candidates should be from the last added period
    False -> filter (\(cdt,cdt') -> (inLastPeriod cdt periods)
                                 || (inLastPeriod cdt' periods))
           $ listToKeys candidates
    where 
        inLastPeriod :: PhyloGroup -> [PhyloPeriodId] -> Bool
        inLastPeriod g prds = (g ^. phylo_groupPeriod) == (last' "makePairs" prds)


phyloGroupMatching :: [[PhyloGroup]] -> Filiation -> Proximity -> Map Date Double -> Double-> PhyloGroup -> PhyloGroup
phyloGroupMatching candidates fil proxi docs thr ego = case pointers of
    Nothing  -> addPointers ego fil TemporalPointer []
    Just pts -> addPointers ego fil TemporalPointer
              $ head' "phyloGroupMatching"
              -- | Keep only the best set of pointers grouped by proximity
              $ groupBy (\pt pt' -> snd pt == snd pt')
              $ reverse $ sortOn snd pts
              -- | Find the first time frame where at leats one pointer satisfies the proximity threshold
    where 
        pointers :: Maybe [Pointer]
        pointers = find (not . null)
                 -- | for each time frame, process the proximity on relevant pairs of targeted groups
                 $ scanl (\acc groups ->
                            let periods = nub $ map (\g' -> g' ^. phylo_groupPeriod) $ concat groups
                                pairs = makePairs (concat groups) periods
                            in  acc ++ ( filter (\(_,proximity) -> filterProximity proxi thr proximity)
                                       $ concat
                                       $ map (\(c,c') ->
                                                -- | process the proximity between the current group and a pair of candidates 
                                                let proximity = toProximity (filterDocs docs periods) proxi ego c c'
                                                in if (c == c')
                                                   then [(getGroupId c,proximity)]
                                                   else [(getGroupId c,proximity),(getGroupId c',proximity)] ) pairs)
                         ) []
                 -- | groups from [[1900],[1900,1901],[1900,1901,1902],...]
                 $ inits candidates
        --------------------------------------                 
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


getCandidates :: Filiation -> PhyloGroup -> [PhyloPeriodId] -> [PhyloGroup] -> [[PhyloGroup]]
getCandidates fil ego pIds targets = 
    case fil of
        ToChilds  -> targets'
        ToParents -> reverse targets'
    where
        targets' :: [[PhyloGroup]]
        targets' = map (\groups' -> filter (\g' -> (not . null) $ intersect (ego ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams)) groups') $ elems
                 $ filterWithKey (\k _ -> elem k pIds) 
                 $ fromListWith (++)
                 $ sortOn (fst . fst)
                 $ map (\g' -> (g' ^. phylo_groupPeriod,[g'])) targets


processMatching :: Int -> [PhyloPeriodId] -> Proximity -> Double -> Map Date Double -> [PhyloGroup] -> [PhyloGroup]
processMatching max' periods proximity thr docs groups =
    let branche  =  map (\group -> 
                        let childs  = getCandidates ToChilds  group
                                                    (getNextPeriods ToChilds  max' (group ^. phylo_groupPeriod) periods) groups
                            parents = getCandidates ToParents group
                                                    (getNextPeriods ToParents max' (group ^. phylo_groupPeriod) periods) groups
                        in phyloGroupMatching parents ToParents proximity docs thr
                         $ phyloGroupMatching childs  ToChilds  proximity docs thr group
                    ) groups
        branche' = branche `using` parList rdeepseq
     in branche'


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
                $ elems $ restrictKeys groups (Set.fromList ids)
           ) graph


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
           -- let next  = 
                        map (\branch -> 
                                let branch' = processMatching frame periods proximity thr docs branch
                                in  groupsToBranches $ fromList $ map (\group -> (getGroupId group, group)) branch'
                           ) branches
            --    next' = next `using` parList rdeepseq
            -- in next



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
        branches = groupsToBranches $ fromList $ map (\group -> (getGroupId group, group))
                 $ trace ("\n" <> "-- | Init temporal matching for " <> show (length $ groups') <> " groups" <> "\n") groups'
        -- | 1) for each group process an initial temporal Matching
        groups' :: [PhyloGroup]
        groups' = processMatching (getTimeFrame $ timeUnit $ getConfig phylo) (getPeriodIds phylo) 
                                  (phyloProximity $ getConfig phylo) (getThresholdInit $ phyloProximity $ getConfig phylo)
                                  (phylo ^. phylo_timeDocs) (getGroupsFromLevel 1 phylo)