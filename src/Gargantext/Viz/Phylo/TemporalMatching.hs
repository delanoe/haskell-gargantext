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

import Data.List (concat, splitAt, tail, sortOn, (++), intersect, null, inits, find, groupBy, scanl, any, nub)
import Data.Map  (Map, fromList, toList, fromListWith, filterWithKey, elems, restrictKeys)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools
import Gargantext.Viz.Phylo.SynchronicClustering

import Control.Lens hiding (Level)


-------------------
-- | Proximity | --
-------------------

-- periodsToNbDocs :: [PhyloPeriodId] -> Phylo -> Double
-- periodsToNbDocs prds phylo = sum $ elems 
--                            $ restrictKeys (phylo ^. phylo_docsByYears)
--                            $ periodsToYears prds 

-- matchWithPairs :: PhyloGroup -> (PhyloGroup,PhyloGroup) -> Phylo -> Double
-- matchWithPairs g1 (g2,g3) p = 
--   let nbDocs = periodsToNbDocs [(getGroupPeriod g1),(getGroupPeriod g2),(getGroupPeriod g3)] p
--       cooc   = if (g2 == g3)
--                 then getGroupCooc g2
--                 else unionWith (+) (getGroupCooc g2) (getGroupCooc g3)
--       ngrams = if (g2 == g3)
--                 then getGroupNgrams g2
--                 else union (getGroupNgrams g2) (getGroupNgrams g3)
--   in processProximity (getPhyloProximity p) nbDocs (getGroupCooc g1) cooc (getGroupNgrams g1) ngrams 


toProximity :: Map Date Double -> PhyloGroup -> PhyloGroup -> PhyloGroup -> Double
toProximity docs group target target' = 
    let nbDocs = sum $ elems docs
    in undefined  

------------------------
-- | Local Matching | --
------------------------


makePairs :: [PhyloGroup] -> [PhyloPeriodId] -> Double -> Map Date Double -> PhyloGroup -> [(PhyloGroup,PhyloGroup)]
makePairs candidates periods thr docs group = case null periods of
    True  -> []
          -- | at least on of the pair candidates should be from the last added period
    False -> filter (\(cdt,cdt') -> (inLastPeriod cdt periods)
                                 || (inLastPeriod cdt' periods))
           $ listToKeys
           -- | remove poor candidates from previous periods
           $ filter (\cdt -> (inLastPeriod cdt periods)
                          || ((toProximity (reframeDocs docs periods) group group cdt) >= thr)) candidates
    where 
        inLastPeriod :: PhyloGroup -> [PhyloPeriodId] -> Bool
        inLastPeriod g prds = (g ^. phylo_groupPeriod) == (last' "makePairs" prds)


phyloGroupMatching :: [[PhyloGroup]] -> Filiation -> Double -> Map Date Double -> PhyloGroup -> PhyloGroup
phyloGroupMatching candidates fil thr docs group = case pointers of
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
                                pairs = makePairs (concat groups) periods thr docs group
                            in  acc ++ ( filter (\(_,proximity) -> proximity >= thr )
                                       $ concat
                                       $ map (\(c,c') ->
                                                -- | process the proximity between the current group and a pair of candidates 
                                                let proximity = toProximity (reframeDocs docs periods) group c c'
                                                in if (c == c')
                                                   then [(getGroupId c,proximity)]
                                                   else [(getGroupId c,proximity),(getGroupId c',proximity)] ) pairs)
                         ) []
                 -- | groups from [[1900],[1900,1901],[1900,1901,1902],...]
                 $ inits candidates




matchGroupToGroups :: [[PhyloGroup]] -> PhyloGroup -> PhyloGroup
matchGroupToGroups candidates group = undefined


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


adaptativeMatching :: Int -> Double -> Double -> Double -> Map Date Double -> [PhyloGroup] -> [PhyloGroup] -> [PhyloPeriodId] -> [PhyloGroup]
adaptativeMatching maxTime thrStep thrMatch thrQua docs groups candidates periods =
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
                            in  matchGroupToGroups parents $ matchGroupToGroups childs group
                      ) groups


temporalMatching :: Phylo -> Phylo
temporalMatching phylo = 
    let branches = fromList $ map (\g -> (getGroupId g, g))
                 $ adaptativeMatching (timeMatching $ getConfig phylo) 0 0 0 
                                      (phylo ^. phylo_timeDocs) 
                                      (getGroupsFromLevel 1 phylo) (getGroupsFromLevel 1 phylo) (getPeriodIds phylo)
    in  updatePhyloGroups 1 branches phylo