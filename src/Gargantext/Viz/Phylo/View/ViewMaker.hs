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

module Gargantext.Viz.Phylo.View.ViewMaker
  where

import Control.Lens     hiding (makeLenses, both, Level)

import Data.List        (notElem,last,head,union,concat,null,nub,(++),init,tail,elemIndex,groupBy,(!!),sortOn,sort,(\\))
import Data.Map         (Map,elems,adjust,unionWith,intersectionWith,fromList,mapKeys,insert,empty)
import Data.Maybe       (isNothing)
import Data.Set         (Set)
import Data.Text        (Text,unwords)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)

import Gargantext.Prelude             hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.View.Display
import Gargantext.Viz.Phylo.View.Filters 
import Gargantext.Viz.Phylo.View.Metrics
import Gargantext.Viz.Phylo.View.Sort
import Gargantext.Viz.Phylo.View.Taggers

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vector


-- | To init a PhyloBranch
initPhyloBranch :: PhyloBranchId -> Text -> PhyloBranch
initPhyloBranch id lbl = PhyloBranch id lbl empty


-- | To init a PhyloEdge
initPhyloEdge :: PhyloGroupId -> [Pointer] -> EdgeType -> [PhyloEdge]
initPhyloEdge id pts et = map (\pt -> PhyloEdge id (fst pt) et (snd pt)) pts


-- | To init a PhyloView
initPhyloView :: Level -> Text -> Text -> Filiation -> Bool -> Phylo -> PhyloView 
initPhyloView lvl lbl dsc fl vb p = PhyloView (getPhyloParams p) lbl dsc fl empty
                                    ([] ++ (phyloToBranches lvl p)) 
                                    ([] ++ (groupsToNodes True vb (getFoundations p) gs))
                                    ([] ++ (groupsToEdges fl PeriodEdge gs))
  where
    --------------------------------------
    gs :: [PhyloGroup]
    gs = getGroupsWithLevel lvl p
    --------------------------------------


-- | To transform a list of PhyloGroups into a list of PhyloNodes
groupsToNodes :: Bool -> Bool -> Vector Ngrams -> [PhyloGroup] -> [PhyloNode]
groupsToNodes isR isV ns gs = map (\g -> let idxs = getGroupNgrams g
                                         in PhyloNode 
                                              (getGroupId g)
                                              (getGroupBranchId g) 
                                              "" idxs
                                              (if isV 
                                                then Just (ngramsToText ns idxs)
                                                else Nothing)
                                              empty 
                                              (if (not isR)
                                                then Just (head $ getGroupLevelParentsId g)
                                                else Nothing)
                                              []
                                  ) gs


-- | To transform a list of PhyloGroups into a list of PhyloEdges
groupsToEdges :: Filiation -> EdgeType -> [PhyloGroup] -> [PhyloEdge]
groupsToEdges fl et gs = case fl of 
                         Complete -> (groupsToEdges Ascendant et gs) ++ (groupsToEdges Descendant et gs)
                         _        -> concat 
                                   $ map (\g -> case fl of
                                                Ascendant  -> case et of 
                                                              PeriodEdge -> initPhyloEdge (getGroupId g) (getGroupPeriodParents g) et
                                                              LevelEdge  -> initPhyloEdge (getGroupId g) (getGroupLevelParents  g) et
                                                Descendant -> case et of 
                                                              PeriodEdge -> initPhyloEdge (getGroupId g) (getGroupPeriodChilds  g) et
                                                              LevelEdge  -> initPhyloEdge (getGroupId g) (getGroupLevelChilds   g) et 
                                                ) gs


-- | To transform a Phylo into a list of PhyloBranch for a given Level
phyloToBranches :: Level -> Phylo -> [PhyloBranch]
phyloToBranches lvl p = map (\id -> initPhyloBranch id "") $ nub $ getBranchIdsWith lvl p


-- | To add recursively a list of PhyloNodes and Edges to PhyloView from a given Level and Depth   
addChildNodes :: Bool -> Level -> Level -> Bool -> Filiation -> Phylo -> PhyloView -> PhyloView
addChildNodes shouldDo lvl lvlMin vb fl p v = 
  if (not shouldDo) || (lvl == lvlMin) 
  then v
  else addChildNodes shouldDo (lvl - 1) lvlMin vb fl p 
     $ v & phylo_viewBranches %~ (++ (phyloToBranches (lvl - 1) p))
         & phylo_viewNodes %~ (++ (groupsToNodes False vb (getFoundations p) gs'))
         & phylo_viewEdges %~ (++ (groupsToEdges fl PeriodEdge gs'))
         & phylo_viewEdges %~ (++ (groupsToEdges Descendant LevelEdge gs ))
         & phylo_viewEdges %~ (++ (groupsToEdges Ascendant LevelEdge  gs'))
    where
      --------------------------------------
      gs :: [PhyloGroup]
      gs = getGroupsWithLevel lvl p
      --------------------------------------  
      gs' :: [PhyloGroup]
      gs' = getGroupsWithLevel (lvl - 1) p
      --------------------------------------


-- | To transform a PhyloQuery into a PhyloView
queryToView :: PhyloQuery -> Phylo -> PhyloView
queryToView q p = processDisplay (q ^. query_display)
                $ processSort (q ^. query_sort) p
                $ processTaggers (q ^. query_taggers) p
                $ processFilters (q ^. query_filters) p
                $ processMetrics (q ^. query_metrics) p 
                $ addChildNodes  (q ^. query_childs) (q ^. query_lvl) (q ^. query_childsDepth) (q ^. query_verbose) (q ^. query_filiation) p
                $ initPhyloView  (q ^. query_lvl) "Phylo2000" "This is a Phylo" (q ^. query_filiation) (q ^. query_verbose) p


-- | dirty params
phyloParams :: PhyloParam
phyloParams = PhyloParam "v0.1" (Software "Gargantext" "v4") ""


-- | To do : effectively get the PhyloParams of a Phylo
getPhyloParams :: Phylo -> PhyloParam 
getPhyloParams p = phyloParams