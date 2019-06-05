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
import Data.List        (concat,nub,groupBy,sortOn,sort)
import Data.Text        (Text)
import Data.Tuple       (fst, snd)
import Data.Vector      (Vector)
import Data.Map         (Map)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.BranchMaker
import qualified Data.Map    as Map
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


-- | To set the label of a PhyloNode as the nth most coocurent terms of its PhyloNodes
nodeLabelCooc :: PhyloView -> Int -> Phylo -> PhyloView
nodeLabelCooc v thr p = over (pv_nodes
                             . traverse)
                             (\n -> let lbl = ngramsToLabel (getFoundationsRoots p)
                                            $ mostOccNgrams thr
                                            $ head' "nodeLabelCooc" $ getGroupsFromIds [getNodeId n] p
                                    in n & pn_label .~ lbl) v


-- | To process a sorted list of Taggers to a PhyloView
processTaggers :: [Tagger] -> Phylo -> PhyloView -> PhyloView
processTaggers ts p v = foldl (\v' t -> case t of
                                        BranchPeakFreq -> branchPeakFreq v' 2 p
                                        -- BranchPeakFreq -> branchPeakCooc v' 3 p
                                        GroupLabelCooc -> nodeLabelCooc  v' 2 p
                                        _              -> panic "[ERR][Viz.Phylo.View.Taggers.processTaggers] tagger not found") v ts

