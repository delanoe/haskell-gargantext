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

module Gargantext.Viz.Phylo.BranchMaker
  where

import Control.Lens     hiding (both, Level)
import Data.List        (concat,nub,(++),tail)
import Data.Tuple       (fst, snd)
import Data.Map         (Map,fromList,toList)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Metrics.Clustering
import Gargantext.Viz.Phylo.Metrics.Proximity
import Gargantext.Viz.Phylo.Tools
-- import Debug.Trace (trace)


-- | To transform a PhyloGraph into a list of PhyloBranches by using the relatedComp clustering
graphToBranches :: Level -> GroupGraph -> Phylo -> [(Int,PhyloGroupId)]
graphToBranches _lvl (nodes,edges) _p = concat
                                    $ map (\(idx,gs) -> map (\g -> (idx,getGroupId g)) gs)
                                    $ zip [1..]
                                    $ relatedComp 0 (head' "branchMaker" nodes) (tail nodes,edges) [] []


mirror :: Ord a => Map (a,a) b -> Map (a,a) b
mirror m = fromList $ concat $ map (\((k,k'),v) -> [((k,k'),v),((k',k),v)]) $ toList m 


-- | To transform a list of PhyloGroups into a PhyloGraph by using a given Proximity mesure
groupsToGraph :: Proximity -> [PhyloGroup] -> Phylo -> GroupGraph
groupsToGraph prox groups p = (groups,edges)
  where
    edges :: GroupEdges
    edges = case prox of
      Filiation          -> (nub . concat) $ map (\g -> (map (\g' -> ((g',g),1)) $ getGroupParents g p)
                                                        ++
                                                        (map (\g' -> ((g,g'),1)) $ getGroupChilds g p)) groups
      WeightedLogJaccard (WLJParams thr sens) -> filter (\(_,v) -> v >= thr)
                                               $ map (\(x,y) -> ((x,y), weightedLogJaccard sens (mirror $ getGroupCooc x) (mirror $ getGroupCooc y)))
                                               $ listToDirectedCombi groups
      Hamming (HammingParams thr) -> filter (\edge -> snd edge <= thr)
                                   $ map (\(x,y) -> ((x,y), hamming (getGroupCooc x) (unifySharedKeys (getGroupCooc x) (getGroupCooc y))))
                                   $ listToDirectedCombi groups
      --_                  -> undefined


-- | To set all the PhyloBranches for a given Level in a Phylo
setPhyloBranches :: Level -> Phylo -> Phylo
setPhyloBranches lvl p = alterGroupWithLevel (\g -> let bIdx = (fst $ head' "branchMaker" $ filter (\b -> snd b == getGroupId g) bs)
                                                     in over (phylo_groupBranchId) (\_ -> Just (lvl,bIdx)) g) lvl p
  where
    --------------------------------------
    bs :: [(Int,PhyloGroupId)]
    bs = graphToBranches lvl graph p
    --------------------------------------
    graph :: GroupGraph
    graph = groupsToGraph Filiation (getGroupsWithLevel lvl p) p
    --------------------------------------
