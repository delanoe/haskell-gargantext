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

import Data.List        (last,head,union,concat,null,nub,(++),init,tail,(!!))
import Data.Map         (Map,elems,adjust,unionWith,intersectionWith)
import Data.Set         (Set)
import Data.Tuple       (fst, snd)

import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.Metrics.Proximity
import Gargantext.Viz.Phylo.Metrics.Clustering

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set


-- | To transform a PhyloGraph into a list of PhyloBranches by using the relatedComp clustering
graphToBranches :: Level -> GroupGraph -> Phylo -> [(Int,PhyloGroupId)]
graphToBranches lvl (nodes,edges) p = concat 
                                    $ map (\(idx,gs) -> map (\g -> (idx,getGroupId g)) gs)
                                    $ zip [1..]
                                    $ relatedComp 0 (head nodes) (tail nodes,edges) [] []


-- | To transform a list of PhyloGroups into a PhyloGraph by using a given Proximity mesure
groupsToGraph :: (Proximity,[Double]) -> [PhyloGroup] -> Phylo -> GroupGraph
groupsToGraph (prox,param) groups p = (groups,edges)
  where 
    edges :: GroupEdges
    edges = case prox of  
      FromPairs          -> (nub . concat) $ map (\g -> (map (\g' -> ((g',g),1)) $ getGroupParents g p)
                                                        ++
                                                        (map (\g' -> ((g,g'),1)) $ getGroupChilds g p)) groups 
      WeightedLogJaccard -> filter (\edge -> snd edge >= (param !! 0)) 
                          $ map (\(x,y) -> ((x,y), weightedLogJaccard 
                                (param !! 1) (getGroupCooc x) 
                                (unifySharedKeys (getGroupCooc x) (getGroupCooc y)))) $ listToDirectedCombi groups
      Hamming            -> filter (\edge -> snd edge <= (param !! 0)) 
                          $ map (\(x,y) -> ((x,y), hamming (getGroupCooc x) 
                                (unifySharedKeys (getGroupCooc x) (getGroupCooc y)))) $ listToDirectedCombi groups                          
      _                  -> undefined 


-- | To set all the PhyloBranches for a given Level in a Phylo
setPhyloBranches :: Level -> Phylo -> Phylo 
setPhyloBranches lvl p = alterGroupWithLevel (\g -> let bIdx = (fst . head) $ filter (\b -> snd b == getGroupId g) bs
                                                     in over (phylo_groupBranchId) (\x -> Just (lvl,bIdx)) g) lvl p
  where
    --------------------------------------
    bs :: [(Int,PhyloGroupId)]
    bs = graphToBranches lvl graph p
    --------------------------------------
    graph :: GroupGraph
    graph = groupsToGraph (FromPairs,[]) (getGroupsWithLevel lvl p) p
    --------------------------------------