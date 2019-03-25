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

module Gargantext.Viz.Phylo.View.Filters
  where

import Control.Lens     hiding (makeLenses, both, Level)

import Data.List        (notElem,last,head,union,concat,null,nub,(++),init,tail,elemIndex,groupBy,(!!),sortOn,sort,(\\))
import Data.Map         (Map,elems,adjust,unionWith,intersectionWith,fromList,mapKeys)
import Data.Maybe       (isNothing)
import Data.Set         (Set)
import Data.Text        (Text,unwords)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)

import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vector


-- | To clean a PhyloView list of Nodes, Edges, etc after having filtered its Branches
cleanNodesEdges :: PhyloView -> PhyloView -> PhyloView
cleanNodesEdges v v' = v' & phylo_viewNodes %~ (filter (\n -> not $ elem (getNodeId n) nIds))
                          & phylo_viewNodes %~ (map (\n -> if isNothing (n ^. phylo_nodeParent)
                                                           then n
                                                           else if elem (getNodeParentId n) nIds
                                                                then n & phylo_nodeParent .~ Nothing
                                                                else n ))
                          & phylo_viewEdges %~ (filter (\e -> (not $ elem (getSourceId e) nIds)
                                                           && (not $ elem (getTargetId e) nIds)))
  where
    --------------------------------------
    nIds :: [PhyloGroupId]
    nIds = map getNodeId
         $ filter (\n -> elem (getNodeBranchId n) bIds)
         $ getNodesInBranches v
    --------------------------------------
    bIds :: [PhyloBranchId]
    bIds = (getViewBranchIds v) \\ (getViewBranchIds v')
    --------------------------------------


-- | To filter all the lonelyBranches (ie: isolated one in time & with a small number of nodes) of a PhyloView
filterLonelyBranch :: Int -> Int -> Int -> [PhyloPeriodId] -> PhyloView -> PhyloView
filterLonelyBranch nbInf nbSup nbNs prds v = cleanNodesEdges v v'
  where
    --------------------------------------
    v' :: PhyloView
    v' = v & phylo_viewBranches %~ (filter (\b -> let ns = filter (\n -> (getBranchId b)  == (getNodeBranchId n)) 
                                                         $ getNodesInBranches v
                                                      prds' = nub $ map (\n -> (fst . fst) $ getNodeId n) ns
                                                  in not (isLone ns prds')))
    --------------------------------------
    isLone :: [PhyloNode] -> [PhyloPeriodId] -> Bool
    isLone ns prds' = (length ns <= nbNs)
                      && notElem (head prds') (take nbInf prds)
                      && notElem (head prds') (take nbSup $ reverse prds)
    --------------------------------------


-- | To process a list of QueryFilter to a PhyloView
processFilters :: [QueryFilter] -> Phylo -> PhyloView -> PhyloView
processFilters fs p v = foldl (\v' f -> case f ^. query_filter of
                                        LonelyBranch -> filterLonelyBranch (round $ (f ^. query_params) !! 0) 
                                                                           (round $ (f ^. query_params) !! 1) 
                                                                           (round $ (f ^. query_params) !! 2) (getPhyloPeriods p) v'
                                        _            -> panic "[ERR][Viz.Phylo.View.Filters.processFilters] filter not found") v fs