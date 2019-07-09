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

module Gargantext.Viz.Phylo.View.Taggers
  where

import Control.Lens     hiding (makeLenses, both, Level)
import Data.List        (concat,nub,groupBy,sortOn,sort, (!!), take, union, (\\))
import Data.Text        (Text)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)
import Data.Map         (Map, (!), empty, unionWith)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.BranchMaker
import Gargantext.Viz.Phylo.Metrics
import qualified Data.Map    as Map
import Control.Parallel.Strategies
-- import Debug.Trace (trace)


-- | To get the nth most frequent Ngrams in a list of PhyloGroups
mostFreqNgrams :: Int -> [PhyloGroup] -> [Int]
mostFreqNgrams thr groups = map fst
                          $ take thr
                          $ reverse
                          $ sortOn snd
                          $ map (\g -> (head' "mostFreqNgrams" g,length g))
                          $ groupBy (==)
                          $ (sort . concat)
                          $ map getGroupNgrams groups


-- | To transform the nth most frequent Ngrams into a label
freqToLabel :: Int -> Vector Ngrams -> [PhyloGroup] -> Text
freqToLabel thr ngs l = ngramsToLabel ngs $ mostFreqNgrams thr l


-- | To get the (nth `div` 2) most cooccuring Ngrams in a PhyloGroup
mostOccNgrams :: Int -> PhyloGroup -> [Int]
mostOccNgrams nth g = (nub . concat)
                    $ map (\((f,s),_d) -> [f,s])
                    $ take nth
                    $ reverse $ sortOn snd
                    $ Map.toList cooc
  where
    cooc :: Map (Int, Int) Double
    cooc = getGroupCooc g


-- | To alter the peak of a PhyloBranch
alterBranchPeak :: (PhyloBranchId,Text) -> PhyloView -> PhyloView
alterBranchPeak (id,lbl) v = over (pv_branches
                                   . traverse)
                                   (\b -> if getBranchId b == id
                                          then b & pb_peak .~ lbl
                                          else b) v


-- | To set the peak of a PhyloBranch as the nth most frequent terms of its PhyloNodes
branchPeakFreq :: PhyloView -> Int -> Phylo -> PhyloView
branchPeakFreq v thr p = foldl (\v' (id,lbl) -> alterBranchPeak (id,lbl) v') v
                        $ map (\(id,ns) -> (id, freqToLabel thr (getFoundationsRoots p)
                                              $ getGroupsFromNodes ns p))
                        $ getNodesByBranches v

branchPeakCooc :: PhyloView -> Int -> Phylo -> PhyloView
branchPeakCooc v nth p = foldl (\v' (id,lbl) -> alterBranchPeak (id,lbl) v') v
                       $ map (\(id,ns) -> (id, ngramsToLabel (getFoundationsRoots p) (getGroupsPeaks (getGroupsFromNodes ns p) nth p) ) ) 
                       $ getNodesByBranches v


getNthMostMeta :: Int -> [Double] -> [Int] -> [Int]
getNthMostMeta nth meta ns = map (\(idx,_) -> (ns !! idx))
                           $ take nth
                           $ reverse
                           $ sortOn snd $ zip [0..] meta 

-- | To set the label of a PhyloNode as the nth most coocurent terms of its PhyloNodes
nodeLabelCooc :: PhyloView -> Int -> Phylo -> PhyloView
nodeLabelCooc v thr p = over (pv_nodes
                             . traverse)
                             (\n -> let g = head' "nodeLabelCooc" $ getGroupsFromIds [getNodeId n] p
                                        lbl = ngramsToLabel (getFoundationsRoots p) $ mostOccNgrams thr g
                                    in n & pn_label .~ lbl) v


-- | To set the label of a PhyloNode as the nth most inclusives terms of its PhyloNodes
nodeLabelInc :: PhyloView -> Int -> Phylo -> PhyloView
nodeLabelInc v thr p = over (pv_nodes
                              . traverse)
                              (\n -> let g = head' "inclusion" $ getGroupsFromIds [getNodeId n] p
                                         lbl = ngramsToLabel (getFoundationsRoots p) 
                                             $ getNthMostMeta thr ((g ^. phylo_groupNgramsMeta) ! "inclusion") (getGroupNgrams g)
                                     in n & pn_label .~ lbl) v    


nodeLabelInc' :: PhyloView -> Int -> Phylo -> PhyloView
nodeLabelInc' v nth p = over (pv_nodes
                               . traverse)
                               (\pn -> let lbl = ngramsToLabel (getFoundationsRoots p)
                                               $ take nth 
                                               $ map (\(_,(_,idx)) -> idx)
                                               $ concat
                                               $ map (\groups -> sortOn (fst . snd) groups)
                                               $ groupBy ((==) `on` fst) $ reverse $ sortOn fst
                                               $ zip ((pn ^. pn_metrics) ! "inclusion")
                                               $ zip ((pn ^. pn_metrics) ! "dynamics") (pn ^. pn_idx)
                                       in pn & pn_label .~ lbl) v     


branchPeakInc :: PhyloView -> Int -> Phylo -> PhyloView  
branchPeakInc v nth p = 
  let labels = map (\(id,nodes) -> 
                    let cooc   = foldl (\mem pn -> unionWith (+) mem (pn ^. pn_cooc)) empty nodes
                        ngrams = sort $ foldl (\mem pn -> union mem (pn ^. pn_idx)) [] nodes
                        inc    = map (\n -> inclusion cooc (ngrams \\ [n]) n) ngrams
                        lbl    = ngramsToLabel (getFoundationsRoots p) $ getNthMostMeta nth inc ngrams
                    in (id, lbl))
             $ getNodesByBranches v
      labels' = labels `using` parList rdeepseq
  in  foldl (\v' (id,lbl) -> alterBranchPeak (id,lbl) v') v labels'                 


-- | To process a sorted list of Taggers to a PhyloView
processTaggers :: [Tagger] -> Phylo -> PhyloView -> PhyloView
processTaggers ts p v = foldl (\v' t -> case t of
                                        BranchPeakFreq   -> branchPeakFreq  v' 2 p
                                        BranchPeakCooc   -> branchPeakCooc  v' 2 p
                                        BranchPeakInc    -> branchPeakInc   v' 2 p
                                        GroupLabelInc    -> nodeLabelInc    v' 2 p
                                        GroupLabelIncDyn -> nodeLabelInc'   v' 2 p
                                        GroupLabelCooc   -> nodeLabelCooc   v' 2 p) v ts

