{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}


module Gargantext.Viz.Phylo.View.ViewMaker
  where

import Control.Lens     hiding (makeLenses, both, Level)
import Data.List        (concat,nub,(++),sort)
import Data.Text        (Text)
import Data.Map         (Map, empty, elems, unionWithKey, fromList)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.Metrics
import Gargantext.Viz.Phylo.View.Display
import Gargantext.Viz.Phylo.View.Filters
import Gargantext.Viz.Phylo.View.Metrics
import Gargantext.Viz.Phylo.View.Sort
import Gargantext.Viz.Phylo.View.Taggers

import qualified Data.Vector.Storable as VS
import Debug.Trace (trace)
import Numeric.Statistics (percentile)

-- | To init a PhyloBranch
initPhyloBranch :: PhyloBranchId -> Text -> PhyloBranch
initPhyloBranch id lbl = PhyloBranch id lbl empty


-- | To init a PhyloEdge
initPhyloEdge :: PhyloGroupId -> [Pointer] -> EdgeType -> [PhyloEdge]
initPhyloEdge id pts et = map (\pt -> PhyloEdge id (fst pt) et (snd pt)) pts


-- | To init a PhyloView
initPhyloView :: Level -> Text -> Text -> Filiation -> Bool -> Phylo -> PhyloView
initPhyloView lvl lbl dsc fl vb p = PhyloView (getPhyloParams p) lbl dsc fl lvl 
                                    (getPhyloPeriods p)
                                    empty
                                    ([] ++ (phyloToBranches lvl p))
                                    ([] ++ (groupsToNodes True vb (getFoundationsRoots p) gs))
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
                                              (g ^. phylo_groupNgramsMeta)
                                              (g ^. phylo_groupCooc)
                                              (if (not isR)
                                                then Just (getGroupLevelParentsId g)
                                                else Nothing)
                                              []
                                  ) gs


-- | To merge edges by keeping the maximum weight
mergeEdges :: [PhyloEdge] -> [PhyloEdge] -> [PhyloEdge]
mergeEdges lAsc lDes = elems
                     $ unionWithKey (\_k vAsc vDes -> vDes & pe_weight .~ (max (vAsc ^. pe_weight) (vDes ^. pe_weight))) mAsc mDes
  where
    --------------------------------------
    mAsc :: Map (PhyloGroupId,PhyloGroupId) PhyloEdge
    mAsc = fromList
         $ map (\(k,e) -> (k, e & pe_source .~ fst k
                                & pe_target .~ snd k))
         $ zip (map (\e -> (e ^. pe_target,e ^. pe_source)) lAsc) lAsc
    --------------------------------------
    mDes :: Map (PhyloGroupId,PhyloGroupId) PhyloEdge
    mDes = fromList
         $ zip (map (\e -> (e ^. pe_source,e ^. pe_target)) lDes) lDes
    --------------------------------------


-- | To transform a list of PhyloGroups into a list of PhyloEdges
groupsToEdges :: Filiation -> EdgeType -> [PhyloGroup] -> [PhyloEdge]
groupsToEdges fl et gs = case fl of
                         Complete -> (groupsToEdges Ascendant et gs) ++ (groupsToEdges Descendant et gs)
                         Merge    -> mergeEdges (groupsToEdges Ascendant et gs) (groupsToEdges Descendant et gs)
                         _        -> concat
                                   $ map (\g -> case fl of
                                                Ascendant  -> case et of
                                                              PeriodEdge -> initPhyloEdge (getGroupId g) (getGroupPeriodParents g) et
                                                              LevelEdge  -> initPhyloEdge (getGroupId g) (getGroupLevelParents  g) et
                                                Descendant -> case et of
                                                              PeriodEdge -> initPhyloEdge (getGroupId g) (getGroupPeriodChilds  g) et
                                                              LevelEdge  -> initPhyloEdge (getGroupId g) (getGroupLevelChilds   g) et
                                                _Type      -> panic "[ERR][Viz.Phylo.View.ViewMaker.groupsToEdges] not implemented"
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
     $ v & pv_branches %~ (++ (phyloToBranches (lvl - 1) p))
         & pv_nodes %~ (++ (groupsToNodes False vb (getFoundationsRoots p) gs'))
         & pv_edges %~ (++ (groupsToEdges fl PeriodEdge gs'))
         & pv_edges %~ (++ (groupsToEdges Descendant LevelEdge gs ))
         & pv_edges %~ (++ (groupsToEdges Ascendant LevelEdge  gs'))
    where
      --------------------------------------
      gs :: [PhyloGroup]
      gs = getGroupsWithLevel lvl p
      --------------------------------------
      gs' :: [PhyloGroup]
      gs' = getGroupsWithLevel (lvl - 1) p
      --------------------------------------


-- | To transform a PhyloQuery into a PhyloView
toPhyloView :: PhyloQueryView -> Phylo -> PhyloView
toPhyloView q p = traceView
                $ processDisplay (q ^. qv_display) (q ^. qv_export)
                $ processSort    (q ^. qv_sort   ) p
                $ processTaggers (q ^. qv_taggers) p
                $ processDynamics
                $ processFilters (q ^. qv_filters) p
                $ processMetrics (q ^. qv_metrics) p
                $ addChildNodes  (q ^. qv_levelChilds) (q ^. qv_lvl) (q ^. qv_levelChildsDepth) (q ^. qv_verbose) (q ^. qv_filiation) p
                $ initPhyloView  (q ^. qv_lvl) (getPhyloTitle p) (getPhyloDescription p) (q ^. qv_filiation) (q ^. qv_verbose) p



-----------------
-- | Taggers | --
-----------------


traceView :: PhyloView -> PhyloView
traceView pv = trace ("------------\n--| View |--\n------------\n\n"
  <> "view level : " <> show (pv ^. pv_level) <> "\n"
  <> show (length $ pv ^. pv_branches) <> " exported branches with " <> show (length $ pv ^. pv_nodes) <> " groups\n"
  <> "groups by branches : " <> show (percentile 25 (VS.fromList lst)) <> " (25%) "
                             <> show (percentile 50 (VS.fromList lst)) <> " (50%) "
                             <> show (percentile 75 (VS.fromList lst)) <> " (75%) "
                             <> show (percentile 90 (VS.fromList lst)) <> " (90%)\n") pv
  where 
    lst = sort $ map (fromIntegral . length . snd) $ getNodesByBranches pv
