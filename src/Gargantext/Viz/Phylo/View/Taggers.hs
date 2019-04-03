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

import Data.List        (last,head,union,concat,null,nub,(++),init,tail,elemIndex,groupBy,(!!),sortOn,sort)
import Data.Map         (Map,elems,adjust,unionWith,intersectionWith,fromList,mapKeys)
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


-- | To transform a list of Ngrams Indexes into a Label
ngramsToLabel :: Vector Ngrams -> [Int] -> Text 
ngramsToLabel ngrams l = unwords $ ngramsToText ngrams l 


-- | To transform a list of Ngrams Indexes into a list of Text 
ngramsToText :: Vector Ngrams -> [Int] -> [Text]
ngramsToText ngrams l = map (\idx -> ngrams Vector.! idx) l


-- | To get the nth most frequent Ngrams in a list of PhyloGroups
mostFreqNgrams :: Int -> [PhyloGroup] -> [Int]
mostFreqNgrams thr groups = map fst 
                          $ take thr 
                          $ reverse 
                          $ sortOn snd 
                          $ map (\g -> (head g,length g)) 
                          $ groupBy (==) 
                          $ (sort . concat) 
                          $ map getGroupNgrams groups


-- | To transform the nth most frequent Ngrams into a label 
freqToLabel :: Int -> Vector Ngrams -> [PhyloGroup] -> Text
freqToLabel thr ngs l = ngramsToLabel ngs $ mostFreqNgrams thr l  


-- | To get the (nth `div` 2) most cooccuring Ngrams in a PhyloGroup 
mostOccNgrams :: Int -> PhyloGroup -> [Int]
mostOccNgrams thr group = (nub . concat ) 
                        $ map (\((f,s),d) -> [f,s]) 
                        $ take (thr `div` 2) 
                        $ reverse $ sortOn snd $ Map.toList $ getGroupCooc group


-- | To alter the label of a PhyloBranch
alterBranchLabel :: (PhyloBranchId,Text) -> PhyloView -> PhyloView
alterBranchLabel (id,lbl) v = over (phylo_viewBranches
                                   . traverse) 
                                   (\b -> if getBranchId b == id
                                          then b & phylo_branchLabel .~ lbl 
                                          else b) v


-- | To set the label of a PhyloBranch as the nth most frequent terms of its PhyloNodes
branchLabelFreq :: PhyloView -> Int -> Phylo -> PhyloView
branchLabelFreq v thr p = foldl (\v' (id,lbl) -> alterBranchLabel (id,lbl) v') v
                        $ map (\(id,ns) -> (id, freqToLabel thr (getPeaksLabels p)
                                              $ getGroupsFromNodes ns p)) 
                        $ getNodesByBranches v


-- | To set the label of a PhyloNode as the nth most coocurent terms of its PhyloNodes
nodeLabelCooc :: PhyloView -> Int -> Phylo -> PhyloView
nodeLabelCooc v thr p = over (phylo_viewNodes
                             . traverse)
                             (\n -> let lbl = ngramsToLabel (getPeaksLabels p)
                                            $ mostOccNgrams thr
                                            $ head $ getGroupsFromIds [getNodeId n] p
                                    in n & phylo_nodeLabel .~ lbl) v


-- | To process a sorted list of Taggers to a PhyloView
processTaggers :: [Tagger] -> Phylo -> PhyloView -> PhyloView 
processTaggers ts p v = foldl (\v' t -> case t of
                                        BranchLabelFreq -> branchLabelFreq v' 2 p
                                        GroupLabelCooc  -> nodeLabelCooc   v' 2 p
                                        _               -> panic "[ERR][Viz.Phylo.View.Taggers.processTaggers] tagger not found") v ts

