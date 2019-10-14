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

import Data.List (concat, splitAt, tail, sortOn, (++), intersect, null, inits, groupBy, scanl, nub, union, dropWhile)
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
                                                            -- $ matchByPeriods ToParents
                                                            -- $ groupByField _phylo_groupPeriod
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


count :: Eq a => a -> [a] -> Int
count x =  length . filter (== x)

termFreq' :: Int -> [PhyloGroup] -> Double
termFreq' term groups = 
    let ngrams = concat $ map _phylo_groupNgrams groups
     in log((fromIntegral $ count term ngrams)
            / (fromIntegral $ length ngrams))

relevantBranches :: Int -> Int -> [[PhyloGroup]] -> [[PhyloGroup]]
relevantBranches term thr branches = 
    filter (\groups -> (length groups >= thr)
                    && (any (\group -> elem term $ group ^. phylo_groupNgrams) groups)) branches

branchCov' :: [PhyloGroup] -> [[PhyloGroup]] -> Double
branchCov' branch branches = 
    (fromIntegral $ length branch) / (fromIntegral $ length $ concat branches)


toRecall :: Double -> Int -> Int -> [[PhyloGroup]] -> Double
toRecall freq term thr branches = 
    -- | given a random term in a phylo
    freq
    -- | for each relevant branches
    * (sum $ map (\branch -> 
                    -- | given its local coverage
                    ((branchCov' branch branches') / (sum $ map (\b -> branchCov' b branches') branches'))
                    -- | compute the local recall
                    * ( (fromIntegral $ length $ filter (\group -> elem term $ group ^. phylo_groupNgrams) branch)
                      / (fromIntegral $ length $ filter (\group -> elem term $ group ^. phylo_groupNgrams) $ concat branches'))) branches')
    where 
        branches' :: [[PhyloGroup]]
        branches' = relevantBranches term thr branches     


toAccuracy :: Double -> Int -> Int -> [[PhyloGroup]] -> Double
toAccuracy freq term thr branches = 
    -- | given a random term in a phylo
    freq
    -- | for each relevant branches
    * (sum $ map (\branch -> 
                    -- | given its local coverage
                    ((branchCov' branch branches') / (sum $ map (\b -> branchCov' b branches') branches'))
                    -- | compute the local accuracy
                    * ( (fromIntegral $ length $ filter (\group -> elem term $ group ^. phylo_groupNgrams) branch)
                      / (fromIntegral $ length branch))) branches')
    where 
        branches' :: [[PhyloGroup]]
        branches' = relevantBranches term thr branches


toRecallWeighted :: Double -> Double -> Double
toRecallWeighted old curr = curr / (old + curr)


toRecall' :: Int -> Map Int Double -> [[PhyloGroup]] -> Double
toRecall' minBranch frequency branches =
  let terms = keys frequency
   in sum $ map (\term -> toRecall (frequency ! term) term minBranch branches) terms 


toPhyloQuality :: Double -> Int -> Map Int Double -> Double -> [[PhyloGroup]] -> Double
toPhyloQuality beta minBranch frequency recall branches =
    if (foldl' (\acc b -> acc && (length b < minBranch)) True branches)
        -- | the local phylo is composed of small branches
        then 0
        else ((1 + beta ** 2) * accuracy * recall)
           / (((beta ** 2) * accuracy + recall))
    where
        terms :: [Int]
        terms = keys frequency
        -- | for each term compute the global accuracy
        accuracy :: Double
        accuracy = sum $ map (\term -> toAccuracy (frequency ! term) term minBranch branches) terms   


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


recursiveMatching :: Proximity -> Double -> Int -> Map Int Double -> Double -> Int -> [PhyloPeriodId] -> Map Date Double -> Double -> Double -> [PhyloGroup] -> [PhyloGroup] 
recursiveMatching proximity beta minBranch frequency egoThr frame periods docs quality recall groups =
  if (length groups == 1)
    then trace ("stop : just one group")
     $ groups
  else if (egoThr >= 1)
    then trace ("stop : thr >= 1") 
     $ groups
  else if (quality > quality')
    then trace ("stop : " <> show(quality) <> " > " <> show(quality'))
     -- $ trace (show(length groups) <> " groups " <> show(length branches'))
     -- $ trace (show(recall) <> " recall " <> show(recall'))
     $ groups
  else trace ("go : " <> show(quality) <> " <= " <> show(quality'))
     $ concat 
     $ map (\branch -> recursiveMatching proximity beta minBranch frequency (egoThr + (getThresholdStep proximity))
                       frame periods docs quality' recall' branch)
     $ branches'
  where
    -- | 2) for each of the possible next branches process the phyloQuality score
    quality' :: Double
    quality' =  toPhyloQuality beta minBranch frequency recall' branches'
    -- | 3) process a new recall weigted by the last one
    recall' :: Double
    recall' = toRecallWeighted recall
            $ toRecall' minBranch frequency branches'
    -- | 1) for each local branch process a temporal matching then find the resulting branches
    branches' :: [[PhyloGroup]]
    branches' =
      let branches = groupsToBranches $ fromList $ map (\g -> (getGroupId g, g))
                   $ phyloBranchMatching frame periods proximity egoThr docs groups
       in branches `using` parList rdeepseq 


temporalMatching :: Phylo -> Phylo
temporalMatching phylo = updatePhyloGroups 1 branches' phylo
  where
    -- | 6) apply the recursive matching
    branches' :: Map PhyloGroupId PhyloGroup
    branches' = fromList
              $ map (\g -> (getGroupId g, g))
              $ traceMatchEnd
              $ concat
              $ map (\branch ->
                  recursiveMatching (phyloProximity $ getConfig phylo)
                                    (_qua_relevance $ phyloQuality $ getConfig phylo)
                                    (_qua_minBranch $ phyloQuality $ getConfig phylo)
                                    frequency
                                    ( (getThresholdInit $ phyloProximity $ getConfig phylo) 
                                    + (getThresholdStep $ phyloProximity $ getConfig phylo))
                                    (getTimeFrame $ timeUnit $ getConfig phylo)
                                    (getPeriodIds phylo)
                                    (phylo ^. phylo_timeDocs) quality recall branch                                                                      
                ) branches
    -- | 5) process the quality score
    quality :: Double
    quality = toPhyloQuality (_qua_relevance $ phyloQuality $ getConfig phylo) (_qua_minBranch $ phyloQuality $ getConfig phylo) frequency recall branches    
    -- | 4) find the recall
    recall :: Double
    recall = toRecall' (_qua_minBranch $ phyloQuality $ getConfig phylo) frequency branches    
    -- | 3) process the constants of the quality score
    frequency :: Map Int Double
    frequency = 
        let terms = ngramsInBranches branches
        in  fromList $ map (\t -> (t, ((termFreq' t $ concat branches) / (sum $ map (\t' -> termFreq' t' $ concat branches) terms)))) terms    
    -- | 2) group into branches
    branches :: [[PhyloGroup]] 
    branches = groupsToBranches $ fromList $ map (\group -> (getGroupId group, group)) groups'    
    -- | 1) for each group process an initial temporal Matching
    groups' :: [PhyloGroup]
    groups' = phyloBranchMatching (getTimeFrame $ timeUnit $ getConfig phylo) (getPeriodIds phylo) 
                                  (phyloProximity $ getConfig phylo) (getThresholdInit $ phyloProximity $ getConfig phylo)
                                  (phylo ^. phylo_timeDocs) 
                                  (traceTemporalMatching $ getGroupsFromLevel 1 phylo)    
