{-|
Module      : Gargantext.Viz.Phylo.SynchronicClustering
Description : Module dedicated to the adaptative synchronic clustering of a Phylo.
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

module Gargantext.Viz.Phylo.SynchronicClustering where

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools
import Gargantext.Viz.Phylo.TemporalMatching (weightedLogJaccard', filterDiago, reduceDiagos)
import Gargantext.Viz.Phylo.PhyloExport (processDynamics)

import Data.List ((++), null, intersect, nub, concat, sort, sortOn, init, all, group, maximum, groupBy)
import Data.Map (Map, fromList, fromListWith, foldlWithKey, (!), insert, empty, restrictKeys, elems, mapWithKey, member, singleton)

import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
-- import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set


-------------------------
-- | New Level Maker | --
-------------------------

mergeBranchIds :: [[Int]] -> [Int]
mergeBranchIds ids = (head' "mergeBranchIds" . sort . mostFreq) ids
  where
    -- | 2) find the most Up Left ids in the hierarchy of similarity
    -- mostUpLeft :: [[Int]] -> [[Int]]
    -- mostUpLeft ids' = 
    --      let groupIds = (map (\gIds -> (length $ head' "gIds" gIds, head' "gIds" gIds)) . groupBy (\id id' -> length id == length id') . sortOn length) ids'
    --          inf = (fst . minimum) groupIds
    --       in map snd $ filter (\gIds -> fst gIds == inf) groupIds
    -- | 1) find the most frequent ids
    mostFreq :: [[Int]] -> [[Int]]
    mostFreq ids' = 
      let groupIds = (map (\gIds -> (length gIds, head' "gIds" gIds)) . group . sort) ids'
          sup = (fst . maximum) groupIds
       in map snd $ filter (\gIds -> fst gIds == sup) groupIds


groupsToBranches' :: Map PhyloGroupId PhyloGroup -> [[PhyloGroup]]
groupsToBranches' groups =
    -- | run the related component algorithm
    let egos  = map (\g -> [getGroupId g] 
                        ++ (map fst $ g ^. phylo_groupPeriodParents)
                        ++ (map fst $ g ^. phylo_groupPeriodChilds) ) $ elems groups
        graph = relatedComponents egos
    -- | update each group's branch id
    in map (\ids ->
        let groups' = elems $ restrictKeys groups (Set.fromList ids)
            bId = mergeBranchIds $ map (\g -> snd $ g ^. phylo_groupBranchId) groups'
         in map (\g -> g & phylo_groupBranchId %~ (\(lvl,_) -> (lvl + 1,bId))) groups') graph

getLastThr :: [PhyloGroup] -> Double
getLastThr childs = maximum $ concat $ map (\g -> (g ^. phylo_groupMeta) ! "thr") childs

mergeGroups :: [Cooc] -> PhyloGroupId -> Map PhyloGroupId PhyloGroupId -> [PhyloGroup] -> PhyloGroup
mergeGroups coocs id mapIds childs = 
    let ngrams = (sort . nub . concat) $ map _phylo_groupNgrams childs
    in PhyloGroup (fst $ fst id) (snd $ fst id) (snd id)  ""
                  (sum $ map _phylo_groupSupport childs)  ngrams
                  (ngramsToCooc ngrams coocs) 
                  ((snd $ fst id),(mergeBranchIds $ map (\g -> snd $ g ^. phylo_groupBranchId) childs))
                  (singleton "thr" [getLastThr childs]) [] (map (\g -> (getGroupId g, 1)) childs)
                  (updatePointers $ concat $ map _phylo_groupPeriodParents childs)
                  (updatePointers $ concat $ map _phylo_groupPeriodChilds  childs)
    where 
        updatePointers :: [Pointer] -> [Pointer]
        updatePointers pointers = map (\(pId,w) -> (mapIds ! pId,w)) pointers


addPhyloLevel :: Level -> Phylo -> Phylo
addPhyloLevel lvl phylo = 
  over ( phylo_periods .  traverse ) 
       (\phyloPrd -> phyloPrd & phylo_periodLevels 
                        %~ (insert (phyloPrd ^. phylo_periodPeriod, lvl) (PhyloLevel (phyloPrd ^. phylo_periodPeriod) lvl empty))) phylo


toNextLevel' :: Phylo -> [PhyloGroup] -> Phylo
toNextLevel' phylo groups = 
    let curLvl = getLastLevel phylo
        oldGroups = fromList $ map (\g -> (getGroupId g, getLevelParentId g)) groups
        newGroups = concat $ groupsToBranches'
                  $ fromList $ map (\g -> (getGroupId g, g))
                  $ foldlWithKey (\acc id groups' ->
                        -- | 4) create the parent group
                        let parent = mergeGroups (elems $ restrictKeys (phylo ^. phylo_timeCooc) $ periodsToYears [(fst . fst) id]) id oldGroups groups'
                        in  acc ++ [parent]) []
                  -- | 3) group the current groups by parentId
                  $ fromListWith (++) $ map (\g -> (getLevelParentId g, [g])) groups

        newPeriods = fromListWith (++) $ map (\g -> (g ^. phylo_groupPeriod, [g])) newGroups
    in  traceSynchronyEnd 
      $ over ( phylo_periods . traverse . phylo_periodLevels . traverse
             -- | 6) update each period at curLvl + 1
             . filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == (curLvl + 1)))
             -- | 7) by adding the parents
             (\phyloLvl -> 
                if member (phyloLvl ^. phylo_levelPeriod) newPeriods
                    then phyloLvl & phylo_levelGroups
                            .~ fromList (map (\g -> (getGroupId g, g)) $ newPeriods ! (phyloLvl ^. phylo_levelPeriod))
                    else phyloLvl)
      -- | 2) add the curLvl + 1 phyloLevel to the phylo
      $ addPhyloLevel (curLvl + 1)
      -- | 1) update the current groups (with level parent pointers) in the phylo
      $ updatePhyloGroups curLvl (fromList $ map (\g -> (getGroupId g, g)) groups) phylo 

--------------------
-- | Clustering | --
--------------------

toPairs :: SynchronyStrategy -> [PhyloGroup] -> [(PhyloGroup,PhyloGroup)]
toPairs strategy groups = case strategy of 
  MergeRegularGroups -> pairs
                      $ filter (\g -> all (== 3) $ (g ^. phylo_groupMeta) ! "dynamics") groups
  MergeAllGroups -> pairs groups
  where 
    pairs :: [PhyloGroup] -> [(PhyloGroup,PhyloGroup)]
    pairs gs = filter (\(g,g') -> (not . null) $ intersect (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams)) (listToCombi' gs)


toDiamonds :: [PhyloGroup] -> [[PhyloGroup]]
toDiamonds groups = foldl' (\acc groups' ->
                        acc ++ ( elems
                               $ Map.filter (\v -> length v > 1)
                               $ fromListWith (++)
                               $ foldl' (\acc' g -> 
                                    acc' ++ (map (\(id,_) -> (id,[g]) ) $ g ^. phylo_groupPeriodChilds)) [] groups')) []
                  $ elems
                  $ Map.filter (\v -> length v > 1)
                  $ fromListWith (++)
                  $ foldl' (\acc g -> acc ++ (map (\(id,_) -> (id,[g]) ) $ g ^. phylo_groupPeriodParents)  ) [] groups


groupsToEdges :: Proximity -> Synchrony -> Double -> Map Int Double -> [PhyloGroup] -> [((PhyloGroup,PhyloGroup),Double)]
groupsToEdges prox sync nbDocs diago groups =
    case sync of
        ByProximityThreshold  thr sens _ strat ->
            filter (\(_,w) -> w >= thr)
          $ toEdges sens
          $ toPairs strat groups         
        ByProximityDistribution sens strat -> 
            let diamonds = sortOn snd 
                         $ toEdges sens $ concat
                         $ map (\gs -> toPairs strat gs) $ toDiamonds groups 
             in take (div (length diamonds) 2) diamonds
    where 
        toEdges :: Double -> [(PhyloGroup,PhyloGroup)] -> [((PhyloGroup,PhyloGroup),Double)]
        toEdges sens edges = 
            case prox of
                WeightedLogJaccard _ _ _ -> map (\(g,g') -> 
                                                 ((g,g'), weightedLogJaccard' sens nbDocs diago
                                                              (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams))) edges
                _ -> undefined  



toRelatedComponents :: [PhyloGroup] -> [((PhyloGroup,PhyloGroup),Double)] -> [[PhyloGroup]]
toRelatedComponents nodes edges = 
  let ref = fromList $ map (\g -> (getGroupId g, g)) nodes
      clusters = relatedComponents $ ((map (\((g,g'),_) -> [getGroupId g, getGroupId g']) edges) ++ (map (\g -> [getGroupId g]) nodes)) 
   in map (\cluster -> map (\gId -> ref ! gId) cluster) clusters 

toParentId :: PhyloGroup -> PhyloGroupId
toParentId child = ((child ^. phylo_groupPeriod, child ^. phylo_groupLevel + 1), child ^. phylo_groupIndex) 


reduceGroups :: Proximity -> Synchrony -> Map Date Double -> Map Date Cooc -> [PhyloGroup] -> [PhyloGroup]
reduceGroups prox sync docs diagos branch = 
    -- | 1) reduce a branch as a set of periods & groups
    let periods = fromListWith (++)
                 $ map (\g -> (g ^. phylo_groupPeriod,[g])) branch
    in  (concat . concat . elems)
      $ mapWithKey (\prd groups -> 
            -- | 2) for each period, transform the groups as a proximity graph filtered by a threshold
            let diago = reduceDiagos $ filterDiago diagos [prd]
                edges = groupsToEdges prox sync ((sum . elems) $ restrictKeys docs $ periodsToYears [prd]) diago groups
             in map (\comp -> 
                    -- | 4) add to each groups their futur level parent group
                    let parentId = toParentId (head' "parentId" comp)
                    in  map (\g -> g & phylo_groupLevelParents %~ (++ [(parentId,1)]) ) comp )
                -- |3) reduce the graph a a set of related components
              $ toRelatedComponents groups edges) periods 


getGroupRealBId :: Double -> PhyloGroup -> [Int]
getGroupRealBId step g = 
  let nb = round(getGroupThr g / step) + 2
   in take nb (snd $ g ^. phylo_groupBranchId)



adjustClustering :: Synchrony -> Double -> [[PhyloGroup]] -> [[PhyloGroup]]
adjustClustering sync step branches = case sync of
  ByProximityThreshold _ _ scope _ -> 
    case scope of 
      SingleBranch -> branches
      SiblingBranches -> groupBy (\g g' -> (init $ getGroupRealBId step g) == (init $ getGroupRealBId step g'))
                       $ sortOn _phylo_groupBranchId $ concat branches
      -- SiblingBranches -> elems $ fromListWith (++) $ map (\b -> ((init . snd . _phylo_groupBranchId) $ head' "adjustClustering" b,b)) branches
      AllBranches -> [concat branches]
  ByProximityDistribution _ _ -> branches



synchronicClustering :: Phylo -> Phylo
synchronicClustering phylo =
    let prox = phyloProximity $ getConfig phylo
        sync = phyloSynchrony $ getConfig phylo
        docs = phylo ^. phylo_timeDocs
        diagos = map coocToDiago $ phylo ^. phylo_timeCooc
        newBranches  = map (\branch -> reduceGroups prox sync docs diagos branch) 
                     $ map processDynamics
                     $ adjustClustering sync (getPhyloThresholdStep phylo)
                     $ phyloToLastBranches 
                     $ traceSynchronyStart phylo
        newBranches' = newBranches `using` parList rdeepseq
     in toNextLevel' phylo $ concat newBranches'


----------------
-- | probes | --
----------------

-- synchronicDistance :: Phylo -> Level -> String
-- synchronicDistance phylo lvl = 
--     foldl' (\acc branch -> 
--              acc <> (foldl' (\acc' period ->
--                               acc' <> let prox  = phyloProximity $ getConfig phylo
--                                           sync  = phyloSynchrony $ getConfig phylo
--                                           docs  = _phylo_timeDocs phylo
--                                           prd   = _phylo_groupPeriod $ head' "distance" period
--                                           edges = groupsToEdges prox 0.1 (_bpt_sensibility sync) 
--                                                   ((sum . elems) $ restrictKeys docs $ periodsToYears [_phylo_groupPeriod $ head' "distance" period]) period
--                                       in foldl' (\mem (_,w) -> 
--                                           mem <> show (prd)
--                                               <> "\t"
--                                               <> show (w)
--                                               <> "\n"
--                                         ) "" edges 
--                      ) ""  $ elems $ groupByField _phylo_groupPeriod branch)
--     ) "period\tdistance\n" $ elems $ groupByField _phylo_groupBranchId $ getGroupsFromLevel lvl phylo