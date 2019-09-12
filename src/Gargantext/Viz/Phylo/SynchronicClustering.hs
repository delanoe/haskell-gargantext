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
import Gargantext.Viz.Phylo.TemporalMatching (weightedLogJaccard)

import Data.List ((++), null, intersect, nub, concat, sort)
import Data.Map (Map, fromList, fromListWith, foldlWithKey, (!), insert, empty, restrictKeys, elems, mapWithKey, member)

import Control.Lens hiding (Level)
import Debug.Trace (trace)


-------------------------
-- | New Level Maker | --
-------------------------

mergeGroups :: [Cooc] -> PhyloGroupId -> [PhyloGroup] -> PhyloGroup
mergeGroups coocs id childs = 
    let ngrams = (sort . nub . concat) $ map _phylo_groupNgrams childs
    in PhyloGroup (fst $ fst id)
                  (snd $ fst id)
                  (snd id) 
                  ""
                  (sum $ map _phylo_groupSupport childs)
                  ngrams
                  (ngramsToCooc ngrams coocs)
                  (((head' "mergeGroups" childs) ^. phylo_groupLevel) + 1, snd ((head' "mergeGroups" childs) ^. phylo_groupBranchId))
                  empty
                  []
                  (map (\g -> (getGroupId g, 1)) childs)
                  (concat $ map _phylo_groupPeriodParents childs)
                  (concat $ map _phylo_groupPeriodChilds childs)


addNewLevel :: Level -> Phylo -> Phylo
addNewLevel lvl phylo = 
  over ( phylo_periods
       .  traverse ) 
  (\phyloPrd ->
      phyloPrd & phylo_periodLevels %~ (insert (phyloPrd ^. phylo_periodPeriod, lvl + 1)
                                               (PhyloLevel (phyloPrd ^. phylo_periodPeriod) (lvl + 1) empty))) phylo

toNextLevel :: Phylo -> [PhyloGroup] -> Phylo
toNextLevel phylo groups = 
    let level   = getLastLevel phylo
        phylo'  = updatePhyloGroups level (fromList $ map (\g -> (getGroupId g, g)) groups) phylo
        nextGroups = fromListWith (++)
                   $ foldlWithKey (\acc k v -> 
                        let group = mergeGroups (elems $ restrictKeys (phylo ^. phylo_timeCooc) $ periodsToYears [fst $ fst k]) k v
                        in  acc ++ [(group ^. phylo_groupPeriod,[group])]) []
                   $ fromListWith (++) $ map (\g -> (fst $ head' "nextGroups" $ g ^. phylo_groupLevelParents,[g])) groups
    in  trace (">>>>>>>>>>>>>>>>>>>>>>>>" <> show (nextGroups)) over ( phylo_periods
             .  traverse
             . phylo_periodLevels
             .  traverse
             . filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == (level + 1))) 
             (\phyloLvl -> if member (phyloLvl ^. phylo_levelPeriod) nextGroups
                           then phyloLvl & phylo_levelGroups .~ fromList ( map (\g -> (getGroupId g,g))
                                                                    $ nextGroups ! (phyloLvl ^. phylo_levelPeriod))
                           else phyloLvl
             ) $ addNewLevel level phylo'   


--------------------
-- | Clustering | --
--------------------


toPairs :: [PhyloGroup] -> [(PhyloGroup,PhyloGroup)]
toPairs groups = filter (\(g,g') -> (not . null) $ intersect (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams))
               $ listToCombi' groups

groupsToEdges :: Proximity -> Double -> Double -> [PhyloGroup] -> [((PhyloGroup,PhyloGroup),Double)]
groupsToEdges prox thr docs groups =
    case prox of
        WeightedLogJaccard sens _ _ -> filter (\(_,w) -> w >= thr)
                                     $ map (\(g,g') -> ((g,g'), weightedLogJaccard sens docs (g ^. phylo_groupCooc) (g' ^. phylo_groupCooc) (g ^. phylo_groupNgrams) (g' ^. phylo_groupNgrams))) 
                                     $ toPairs groups
        _ -> undefined 


toRelatedComponents :: [PhyloGroup] -> [((PhyloGroup,PhyloGroup),Double)] -> [[PhyloGroup]]
toRelatedComponents nodes edges = relatedComponents $ ((map (\((g,g'),_) -> [g,g']) edges) ++ (map (\g -> [g]) nodes)) 


reduceBranch :: Proximity -> Double -> Map Date Double -> [PhyloGroup] -> [PhyloGroup]
reduceBranch prox thr docs branch = 
    -- | 1) reduce a branch as a set of periods & groups
    let periods = fromListWith (++)
                 $ map (\g -> (g ^. phylo_groupPeriod,[g])) branch
    in  (concat . concat . elems)
      $ mapWithKey (\prd groups ->
            -- | 2) for each period, transform the groups as a proximity graph filtered by a threshold
            let edges = groupsToEdges prox thr ((sum . elems) $ restrictKeys docs $ periodsToYears [prd]) groups
            in  map (\(idx,comp) ->
                    -- | 4) add to each groups their futur level parent group
                    let parentId = (((head' "reduceBranch" comp) ^. phylo_groupPeriod, 1 + (head' "reduceBranch" comp) ^. phylo_groupLevel), idx)
                    in  map (\g -> g & phylo_groupLevelParents %~ (++ [(parentId,1)]) ) comp )
                -- |3) reduce the graph a a set of related components
              $ zip [1..] (toRelatedComponents groups edges)) periods 


synchronicClustering :: Phylo -> Phylo
synchronicClustering phylo = 
    case (phyloSynchrony $ getConfig phylo) of
        ByProximityThreshold thr -> toNextLevel phylo  
                                  $ concat 
                                  $ map (\branch -> reduceBranch (phyloProximity $ getConfig phylo) thr (phylo ^. phylo_timeDocs) branch) 
                                  $ phyloToLastBranches phylo
        ByProximityDistribution  -> undefined 