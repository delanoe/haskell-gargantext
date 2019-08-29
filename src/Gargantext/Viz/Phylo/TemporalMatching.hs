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

import Data.List (concat, splitAt, tail, sortOn, (++), intersect, null, inits, find, groupBy, scanl, any, nub, union)
import Data.Map  (Map, fromList, toList, fromListWith, filterWithKey, elems, restrictKeys, unionWith, intersectionWith)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools
import Gargantext.Viz.Phylo.SynchronicClustering

import Control.Lens hiding (Level)


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
    WeightedLogJaccard sens -> weightedLogJaccard sens docs cooc cooc' ngrams ngrams'
    Hamming                 -> undefined


-- | To process the proximity between a current group and a pair of targets group
toProximity :: Map Date Double -> Proximity -> PhyloGroup -> PhyloGroup -> PhyloGroup -> Double
toProximity docs proximity group target target' = 
    let docs'  = sum $ elems docs
        cooc   = if target == target'
                 then (target ^. phylo_groupCooc)
                 else sumCooc (target ^. phylo_groupCooc) (target' ^. phylo_groupCooc)
        ngrams = if target == target'
                 then (target ^. phylo_groupNgrams)
                 else union (target ^. phylo_groupNgrams) (target' ^. phylo_groupNgrams)
    in pickProximity proximity docs' (group ^. phylo_groupCooc) cooc (group ^. phylo_groupNgrams) ngrams 


------------------------
-- | Local Matching | --
------------------------


-- | Find pairs of valuable candidates to be matched
makePairs :: [PhyloGroup] -> [PhyloPeriodId] -> Double -> Map Date Double -> Proximity -> PhyloGroup -> [(PhyloGroup,PhyloGroup)]
makePairs candidates periods thr docs proximity group = case null periods of
    True  -> []
          -- | at least on of the pair candidates should be from the last added period
    False -> filter (\(cdt,cdt') -> (inLastPeriod cdt periods)
                                 || (inLastPeriod cdt' periods))
           $ listToKeys
           -- | remove poor candidates from previous periods
           $ filter (\cdt -> (inLastPeriod cdt periods)
                          || ((toProximity (reframeDocs docs periods) proximity group group cdt) >= thr)) candidates
    where 
        inLastPeriod :: PhyloGroup -> [PhyloPeriodId] -> Bool
        inLastPeriod g prds = (g ^. phylo_groupPeriod) == (last' "makePairs" prds)


phyloGroupMatching :: [[PhyloGroup]] -> Filiation -> Double -> Map Date Double -> Proximity -> PhyloGroup -> PhyloGroup
phyloGroupMatching candidates fil thr docs proxi group = case pointers of
    Nothing  -> addPointers group fil TemporalPointer []
    Just pts -> addPointers group fil TemporalPointer
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
                                pairs = makePairs (concat groups) periods thr docs proxi group
                            in  acc ++ ( filter (\(_,proximity) -> proximity >= thr )
                                       $ concat
                                       $ map (\(c,c') ->
                                                -- | process the proximity between the current group and a pair of candidates 
                                                let proximity = toProximity (reframeDocs docs periods) proxi group c c'
                                                in if (c == c')
                                                   then [(getGroupId c,proximity)]
                                                   else [(getGroupId c,proximity),(getGroupId c',proximity)] ) pairs)
                         ) []
                 -- | groups from [[1900],[1900,1901],[1900,1901,1902],...]
                 $ inits candidates


-----------------------------
-- | Adaptative Matching | --
-----------------------------


getNextPeriods :: Filiation -> Int -> PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
getNextPeriods fil max pId pIds = 
    case fil of 
        ToChilds  -> take max $ (tail . snd) $ splitAt (elemIndex' pId pIds) pIds
        ToParents -> take max $ (reverse . fst) $ splitAt (elemIndex' pId pIds) pIds


getCandidates :: Filiation -> PhyloGroup -> [PhyloPeriodId] -> [PhyloGroup] -> [[PhyloGroup]]
getCandidates fil g pIds targets = 
    case fil of
        ToChilds  -> targets'
        ToParents -> reverse targets'
    where
        targets' :: [[PhyloGroup]]
        targets' = map (\groups' -> filter (\g' -> (not . null) $ intersect (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams)) groups') $ elems
                 $ filterWithKey (\k _ -> elem k pIds) 
                 $ fromListWith (++)
                 $ sortOn (fst . fst)
                 $ map (\g' -> (g' ^. phylo_groupPeriod,[g'])) targets


shouldBreak :: Double -> [(Double,[PhyloGroup])] -> Bool
shouldBreak thr branches = any (\(quality,_) -> quality < thr) branches


toBranchQuality :: [[PhyloGroup]] -> [(Double,[PhyloGroup])]
toBranchQuality branches = undefined


reframeDocs :: Map Date Double -> [PhyloPeriodId] -> Map Date Double
reframeDocs docs periods = restrictKeys docs $ periodsToYears periods


-- findGhostLinks :: [Link] -> [[Link]] -> Map PhyloGroupId 

adaptativeMatching :: Int -> Double -> Double -> Double -> Map Date Double -> Proximity -> [PhyloGroup] -> [PhyloGroup] -> [PhyloPeriodId] -> [PhyloGroup]
adaptativeMatching maxTime thrStep thrMatch thrQua docs proximity groups candidates periods =
    -- | check if we should break some of the new branches or not
    case shouldBreak thrQua branches' of
        True  -> concat $ map (\(s,b) -> 
                                    if s >= thrQua
                                    -- | we keep the branch as it is
                                    then b
                                    -- | we break the branch using an increased temporal matching threshold
                                    else let nextGroups = undefined
                                             nextCandidates = undefined
                                             nextPeriods = undefined
                                         in  adaptativeMatching maxTime thrStep (thrMatch + thrStep) thrQua 
                                                                (reframeDocs docs nextPeriods)
                                                                proximity
                                                                nextGroups nextCandidates nextPeriods
                              ) branches'
        -- | the quality of all the new branches is sufficient
        False -> concat branches
    where
        -- | 3) process a quality score for each new branch
        branches' :: [(Double,[PhyloGroup])]
        branches' = toBranchQuality branches
        -- | 2) group the new groups into branches 
        branches :: [[PhyloGroup]]
        branches = relatedComponents groups'
        -- | 1) connect each group to its parents and childs
        groups' :: [PhyloGroup]
        groups' = map (\group ->
                            let childs  = getCandidates ToChilds group
                                                        (getNextPeriods ToChilds maxTime (group ^. phylo_groupPeriod) periods) candidates
                                parents = getCandidates ToParents group
                                                        (getNextPeriods ToParents maxTime (group ^. phylo_groupPeriod) periods) candidates
                            -- | match the group to its possible childs then parents
                            in phyloGroupMatching parents ToParents thrMatch docs proximity
                             $ phyloGroupMatching childs  ToChilds  thrMatch docs proximity group
                      ) groups


temporalMatching :: Phylo -> Phylo
temporalMatching phylo = 
    let branches = fromList $ map (\g -> (getGroupId g, g))
                 $ adaptativeMatching (maxTimeMatch $ getConfig phylo) 0 0 0 
                                      (phylo ^. phylo_timeDocs)
                                      (phyloProximity $ getConfig phylo)
                                      (getGroupsFromLevel 1 phylo) (getGroupsFromLevel 1 phylo) (getPeriodIds phylo)
    in  updatePhyloGroups 1 branches phylo