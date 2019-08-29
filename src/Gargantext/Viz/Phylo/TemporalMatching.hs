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
import Data.Map  (Map, fromList, toList, fromListWith, filterWithKey, elems, restrictKeys, unionWith, intersectionWith, member, (!))

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools
import Gargantext.Viz.Phylo.SynchronicClustering

import Control.Lens hiding (Level)

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
makePairs :: [PhyloGroup] -> [PhyloPeriodId] -> Map Date Double -> PhyloGroup -> [(PhyloGroup,PhyloGroup)]
makePairs candidates periods docs group = case null periods of
    True  -> []
          -- | at least on of the pair candidates should be from the last added period
    False -> filter (\(cdt,cdt') -> (inLastPeriod cdt periods)
                                 || (inLastPeriod cdt' periods))
           $ listToKeys candidates
    where 
        inLastPeriod :: PhyloGroup -> [PhyloPeriodId] -> Bool
        inLastPeriod g prds = (g ^. phylo_groupPeriod) == (last' "makePairs" prds)


phyloGroupMatching :: [[PhyloGroup]] -> Filiation -> Proximity -> Map Date Double -> PhyloGroup -> PhyloGroup
phyloGroupMatching candidates fil proxi docs group = case pointers of
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
                                pairs = makePairs (concat groups) periods docs group
                            in  acc ++ ( concat
                                       $ map (\(c,c') ->
                                                -- | process the proximity between the current group and a pair of candidates 
                                                let proximity = toProximity (filterDocs docs periods) proxi group c c'
                                                in if (c == c')
                                                   then [(getGroupId c,proximity)]
                                                   else [(getGroupId c,proximity),(getGroupId c',proximity)] ) pairs)
                         ) []
                 -- | groups from [[1900],[1900,1901],[1900,1901,1902],...]
                 $ inits candidates
        --------------------------------------                 
        filterDocs :: Map Date Double -> [PhyloPeriodId] -> Map Date Double
        filterDocs d pds = restrictKeys d $ periodsToYears pds



------------------
-- | Pointers | --
------------------


-- ghostHunter :: [[PhyloGroup]] -> [[PhyloGroup]]
-- ghostHunter branches = 
--     map (\branch -> 
--         -- | il manque une référence au group source de chaque pointer
--         let pointers = elems $ fromList
--                      $ map (\pt -> (groupIds ! (fst pt),pt))
--                      $ filter (\pt -> member (fst pt) groupIds) $ concat $ map (\g -> g ^. phylo_groupGhostPointers) branch

--         in undefined
--         ) branches
--     where
--         groupIds :: Map PhyloGroupId Int 
--         groupIds = fromList $ map (\g -> (getGroupId g, last' "ghostHunter" $ snd $ g ^. phylo_groupBranchId)) $ concat branches
--         --------------------------------------
--         selectBest :: [Pointers] -> [Pointers]
--         se



filterPointers :: Double -> [PhyloGroup] -> [PhyloGroup]
filterPointers thr groups =
    map (\group -> 
            let ghosts = filter (\(_,w) -> w < thr) $ group ^. phylo_groupPeriodParents
            in  group & phylo_groupPeriodParents %~ (filter (\(_,w) -> w >= thr))
                      & phylo_groupPeriodChilds  %~ (filter (\(_,w) -> w >= thr))
                      & phylo_groupGhostPointers %~ (++ ghosts) 
        ) groups


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



-- findGhostLinks :: [Link] -> [[Link]] -> Map PhyloGroupId 

adaptativeMatching :: Proximity -> Double -> Double -> [PhyloGroup] -> [PhyloGroup]
adaptativeMatching proximity thr thrQua groups =
    -- | check if we should break some of the new branches or not
    case shouldBreak thrQua branches' of
        True  -> concat $ map (\(s,b) -> 
                                    if s >= thrQua
                                    -- | we keep the branch as it is
                                    then b
                                    -- | we break the branch using an increased temporal matching threshold
                                    else let nextGroups = undefined
                                         in  adaptativeMatching proximity (thr + (getThresholdStep proximity)) thrQua nextGroups
                              ) branches'
        -- | the quality of all the new branches is sufficient
        False -> concat branches
    where
        -- | 3) process a quality score for each new branch
        branches' :: [(Double,[PhyloGroup])]
        branches' = toBranchQuality branches
        -- | 2) group the new groups into branches 
        branches :: [[PhyloGroup]]
        branches = groupsToBranches $ fromList $ map (\group -> (getGroupId group, group)) groups'
        -- | 1) filter the pointers of each groups regarding the current state of the quality threshold
        groups' :: [PhyloGroup]
        groups' = filterPointers thr groups


temporalMatching :: Phylo -> Phylo
temporalMatching phylo = updatePhyloGroups 1 branches phylo
    where
        -- | 4) find the ghost links and postprocess the branches
        branches' :: Map PhyloGroupId PhyloGroup
        branches' = undefined
        -- | 3) run the adaptative matching to find the best repartition among branches
        branches :: Map PhyloGroupId PhyloGroup
        branches = fromList 
                 $ map (\g -> (getGroupId g, g))
                 $ adaptativeMatching proximity (getThresholdInit proximity) (phyloQuality $ getConfig phylo) groups'
        -- | 2) for each group process an initial temporal Matching
        groups' :: [PhyloGroup]
        groups' = 
            let maxTime = getTimeFrame $ timeUnit $ getConfig phylo
                periods = getPeriodIds phylo
                docs    = phylo ^. phylo_timeDocs
                --------------------------------------
            in map (\group ->
                        let childs  = getCandidates ToChilds  group
                                                    (getNextPeriods ToChilds  maxTime (group ^. phylo_groupPeriod) periods) groups
                            parents = getCandidates ToParents group
                                                    (getNextPeriods ToParents maxTime (group ^. phylo_groupPeriod) periods) groups
                        in phyloGroupMatching parents ToParents proximity docs
                         $ phyloGroupMatching childs  ToChilds  proximity docs group
                   ) groups
        -- | 1) start with all the groups from a given level
        groups :: [PhyloGroup]
        groups = getGroupsFromLevel 1 phylo
        --------------------------------------
        proximity :: Proximity 
        proximity = phyloProximity $ getConfig phylo