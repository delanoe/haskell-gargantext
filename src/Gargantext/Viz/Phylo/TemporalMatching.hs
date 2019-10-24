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

import Data.List (concat, splitAt, tail, sortOn, (++), intersect, null, inits, groupBy, scanl, nub, union, dropWhile, partition, or)
import Data.Map  (Map, fromList, elems, restrictKeys, unionWith, intersectionWith, findWithDefault, keys, (!), filterWithKey)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools

-- import Prelude (logBase)
import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Debug.Trace (trace)

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
    -- coocInter = elems $ map (/docs) $ intersectionWith (+) cooc cooc'       
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

toLastPeriod :: Filiation -> [PhyloPeriodId] -> PhyloPeriodId
toLastPeriod fil periods = case fil of
    ToParents -> head' "toLastPeriod" (sortOn fst periods)
    ToChilds  -> last' "toLastPeriod" (sortOn fst periods)


toLazyPairs :: [Pointer] -> Filiation -> Double -> Proximity -> PhyloPeriodId -> [(PhyloGroup,PhyloGroup)] -> [(PhyloGroup,PhyloGroup)]
toLazyPairs pointers fil thr prox prd pairs = 
    if null pointers then pairs
        else let rest = filterPointers prox thr pointers
              in if null rest
                    then let prd' = toLastPeriod fil (map (fst . fst . fst) pointers)
                          in if prd' == prd
                             then []
                             else filter (\(g,g') -> 
                                case fil of
                                     ToParents -> ((fst $ g  ^. phylo_groupPeriod) < (fst prd'))
                                               || ((fst $ g' ^. phylo_groupPeriod) < (fst prd'))
                                     ToChilds  -> ((fst $ g  ^. phylo_groupPeriod) > (fst prd'))
                                               || ((fst $ g' ^. phylo_groupPeriod) > (fst prd'))) pairs 
                    else []


-- | Find pairs of valuable candidates to be matched
makePairs' :: PhyloGroup -> [PhyloGroup] -> [PhyloPeriodId] -> [Pointer] -> Filiation -> Double -> Proximity -> Map Date Double -> [(PhyloGroup,PhyloGroup)]
makePairs' ego candidates periods pointers fil thr prox docs = 
    case null periods of 
        True  -> []
        False -> toLazyPairs pointers fil thr prox lastPrd
                -- | at least on of the pair candidates should be from the last added period 
               $ filter (\(g,g') -> ((g  ^. phylo_groupPeriod) == lastPrd)
                                 || ((g' ^. phylo_groupPeriod) == lastPrd))
               $ listToKeys 
               $ filter (\g -> (g ^. phylo_groupPeriod == lastPrd)
                            || ((toProximity docs prox ego ego g) >= thr)) candidates 
    where 
        lastPrd :: PhyloPeriodId
        lastPrd = toLastPeriod fil periods


filterPointers :: Proximity -> Double -> [Pointer] -> [Pointer]
filterPointers proxi thr pts = filter (\(_,w) -> filterProximity proxi thr w) pts


phyloGroupMatching :: [[PhyloGroup]] -> Filiation -> Proximity -> Map Date Double -> Double -> PhyloGroup -> PhyloGroup
phyloGroupMatching candidates fil proxi docs thr ego = 
    case null nextPointers of
            -- | let's find new pointers
            True  -> if null $ filterPointers proxi thr $ getPeriodPointers fil ego
                        then addPointers ego fil TemporalPointer []
                        -- | or keep the old ones
                        else addPointers ego fil TemporalPointer
                           $ filterPointers proxi thr $ getPeriodPointers fil ego
            False -> addPointers ego fil TemporalPointer
                   $ head' "phyloGroupMatching"
                   -- | Keep only the best set of pointers grouped by proximity
                   $ groupBy (\pt pt' -> snd pt == snd pt')
                   $ reverse $ sortOn snd $ head' "pointers" 
                   $ nextPointers
                   -- | Find the first time frame where at leats one pointer satisfies the proximity threshold
    where
        nextPointers :: [[Pointer]]
        nextPointers = take 1
                 $ dropWhile (null)
                 -- | for each time frame, process the proximity on relevant pairs of targeted groups
                 $ scanl (\acc groups ->
                            let periods = nub $ map _phylo_groupPeriod $ concat groups
                                docs' = (filterDocs docs ([ego ^. phylo_groupPeriod] ++ periods))
                                pairs = makePairs' ego (concat groups) periods (getPeriodPointers fil ego) fil thr proxi docs
                            in acc ++ ( filterPointers proxi thr 
                                        $ concat
                                        $ map (\(c,c') ->
                                            -- | process the proximity between the current group and a pair of candidates 
                                            let proximity = toProximity docs' proxi ego c c'
                                            in if (c == c')
                                               then [(getGroupId c,proximity)]
                                               else [(getGroupId c,proximity),(getGroupId c',proximity)] ) pairs )) []
                 $ inits candidates -- | groups from [[1900],[1900,1901],[1900,1901,1902],...] 


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
                                                            $ matchByPeriods
                                                            $ groupByField _phylo_groupPeriod branch
    where
        --------------------------------------
        matchByPeriods :: Map PhyloPeriodId [PhyloGroup] -> [PhyloGroup]
        matchByPeriods branch' = foldl' (\acc prd ->
            let periodsPar = getNextPeriods ToParents frame prd periods
                periodsChi = getNextPeriods ToChilds frame prd periods
                candidatesPar = map (\prd' -> findWithDefault [] prd' branch') periodsPar
                candidatesChi = map (\prd' -> findWithDefault [] prd' branch') periodsChi
                docsPar = filterDocs docs ([prd] ++ periodsPar)
                docsChi = filterDocs docs ([prd] ++ periodsChi)
                egos  = map (\ego -> phyloGroupMatching (getCandidates ToParents ego candidatesPar) ToParents proximity docsPar thr
                                   $ phyloGroupMatching (getCandidates ToChilds  ego candidatesChi) ToChilds  proximity docsChi thr ego)
                      $ findWithDefault [] prd branch'
                egos' = egos `using` parList rdeepseq
             in acc ++ egos' ) [] periods


-----------------------
-- | Phylo Quality | --
-----------------------


relevantBranches :: Int -> [[PhyloGroup]] -> [[PhyloGroup]]
relevantBranches term branches = 
    filter (\groups -> (any (\group -> elem term $ group ^. phylo_groupNgrams) groups)) branches

fScore :: Double -> Int -> [PhyloGroup] -> [[PhyloGroup]] -> Double
fScore beta i bk bks = 
  let recall = ( (fromIntegral $ length $ filter (\g -> elem i $ g ^. phylo_groupNgrams) bk)
               / (fromIntegral $ length $ filter (\g -> elem i $ g ^. phylo_groupNgrams) $ concat bks))
      accuracy = ( (fromIntegral $ length $ filter (\g -> elem i $ g ^. phylo_groupNgrams) bk)
                 / (fromIntegral $ length bk))
   in ((1 + beta ** 2) * accuracy * recall)
    / (((beta ** 2) * accuracy + recall))


wk :: [PhyloGroup] -> Double
wk bk = fromIntegral $ length bk


toPhyloQuality' :: Double -> Map Int Double -> [[PhyloGroup]] -> Double
toPhyloQuality' beta freq branches =
  if (null branches)
    then 0
    else sum 
       $ map (\i -> 
          let bks = relevantBranches i branches
           in (freq ! i) * (sum $ map (\bk -> ((wk bk) / (sum $ map wk bks)) * (fScore beta i bk bks)) bks))
       $ keys freq


-----------------------------
-- | Adaptative Matching | --
-----------------------------


groupsToBranches :: Map PhyloGroupId PhyloGroup -> [[PhyloGroup]]
groupsToBranches groups =
    -- | run the related component algorithm
    let egos = groupBy (\gs gs' -> (fst $ fst $ head' "egos" gs) == (fst $ fst $ head' "egos" gs'))
             $ sortOn  (\gs -> fst $ fst $ head' "egos" gs)
             $ map (\group -> [getGroupId group] 
                            ++ (map fst $ group ^. phylo_groupPeriodParents)
                            ++ (map fst $ group ^. phylo_groupPeriodChilds) ) $ elems groups
        -- | first find the related components by inside each ego's period
        graph' = map relatedComponents egos
        -- | then run it for the all the periods
        graph  = zip [1..] 
               $ relatedComponents $ concat (graph' `using` parList rdeepseq)
    -- | update each group's branch id
    in map (\(bId,ids) ->
        let groups'  = map (\group -> group & phylo_groupBranchId %~ (\(lvl,lst) -> (lvl,lst ++ [bId])))
                     $ elems $ restrictKeys groups (Set.fromList ids)
         in groups' `using` parList rdeepseq ) graph


reduceFrequency :: Map Int Double -> [[PhyloGroup]] -> Map Int Double
reduceFrequency frequency branches = 
  restrictKeys frequency (Set.fromList $ (nub . concat) $ map _phylo_groupNgrams $ concat branches)


seqMatching :: Proximity -> Double -> Map Int Double -> Int -> Double -> Int -> Map Date Double -> [PhyloPeriodId] -> [([PhyloGroup],Bool)] -> ([PhyloGroup],Bool) -> [([PhyloGroup],Bool)] -> [([PhyloGroup],Bool)]
seqMatching proximity beta frequency minBranch egoThr frame docs periods done ego rest =
  -- | 1) keep or not the new division of ego
  let done' = done ++ (if snd ego 
                        then (if ((null (fst ego')) || (quality > quality')) 
                               then trace ("  ✗ F(β) = " <> show(quality) <> " (vs) " <> show(quality')
                                        <> "  | "  <> show(length $ fst ego) <> " groups : " 
                                        <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                                        <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")
                                  $ [(fst ego,False)] 
                               else trace ("  ✓ F(β) = " <> show(quality) <> " (vs) " <> show(quality')
                                        <> "  | "  <> show(length $ fst ego) <> " groups : " 
                                        <> "  |✓ " <> show(length $ fst ego') <> show(map length $ fst ego')
                                        <> "  |✗ " <> show(length $ snd ego') <> "[" <> show(length $ concat $ snd ego') <> "]")     
                                  $ ((map (\e -> (e,True)) (fst ego')) ++ (map (\e -> (e,False)) (snd ego'))))
                        else [ego])
  in 
    -- | 2) if there is no more branches in rest then return else continue    
    if null rest 
      then done'
      else seqMatching proximity beta frequency minBranch egoThr frame docs periods
                       done' (head' "seqMatching" rest) (tail' "seqMatching" rest) 
  where
    --------------------------------------
    quality :: Double 
    quality = toPhyloQuality' beta frequency ((map fst done) ++ [fst ego] ++ (map fst rest))
    --------------------------------------
    ego' :: ([[PhyloGroup]],[[PhyloGroup]])
    ego' = 
      let branches  = groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
                    $ phyloBranchMatching frame periods proximity egoThr docs (fst ego)
          branches' = branches `using` parList rdeepseq
       in partition (\b -> (length $ nub $ map _phylo_groupPeriod b) >= minBranch) branches'
    --------------------------------------
    quality' :: Double
    quality' = toPhyloQuality' beta frequency
                                    ((map fst done) ++ (fst ego') ++ (snd ego') ++ (map fst rest))


recursiveMatching' :: Proximity -> Double -> Int -> Map Int Double -> Double -> Int -> [PhyloPeriodId] -> Map Date Double -> [([PhyloGroup],Bool)] -> [([PhyloGroup],Bool)]
recursiveMatching' proximity beta minBranch frequency egoThr frame periods docs branches =
  if (egoThr >= 1) || ((not . or) $ map snd branches)
    then branches
    else 
      let branches' = seqMatching proximity beta frequency minBranch egoThr frame docs periods 
                                  [] (head' "recursiveMatching" branches) (tail' "recursiveMatching" branches)
          frequency' = reduceFrequency frequency (map fst branches')
       in recursiveMatching' proximity beta minBranch frequency' (egoThr + (getThresholdStep proximity))  frame periods docs branches'


temporalMatching :: Phylo -> Phylo 
temporalMatching phylo = updatePhyloGroups 1 
                          (fromList $ map (\g -> (getGroupId g,g)) $ traceMatchEnd $ concat branches)
                          phylo
  where
    -- | 2) init the recursiveMatching      
    branches :: [[PhyloGroup]]
    branches = map fst
             $ recursiveMatching' (phyloProximity $ getConfig phylo)
                                  (_qua_granularity $ phyloQuality $ getConfig phylo)
                                  (_qua_minBranch $ phyloQuality $ getConfig phylo)
                                  (phylo ^. phylo_termFreq)
                                  (getThresholdInit $ phyloProximity $ getConfig phylo)
                                  (getTimeFrame $ timeUnit $ getConfig phylo)
                                  (getPeriodIds phylo)
                                  (phylo ^. phylo_timeDocs)
                                  [(groups,True)]    
    -- | 1) for each group process an initial temporal Matching
    groups :: [PhyloGroup]
    groups = phyloBranchMatching (getTimeFrame $ timeUnit $ getConfig phylo) (getPeriodIds phylo) 
                                 (phyloProximity $ getConfig phylo) (getThresholdInit $ phyloProximity $ getConfig phylo)
                                 (phylo ^. phylo_timeDocs) 
                                 (traceTemporalMatching $ getGroupsFromLevel 1 phylo)