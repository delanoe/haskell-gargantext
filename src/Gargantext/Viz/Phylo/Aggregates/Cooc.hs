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

module Gargantext.Viz.Phylo.Aggregates.Cooc
  where

import Data.List        (union,concat,nub)
import Data.Map         (Map,elems,adjust,filterWithKey)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools
import qualified Data.Map    as Map
import qualified Data.Set    as Set


-- | To transform the Fis into a full coocurency Matrix in a Phylo
fisToCooc :: Map (Date, Date) [PhyloFis] -> Phylo -> Map (Int, Int) Double
fisToCooc m p = map (/docs)
              $ foldl (\mem x -> adjust (+1) x mem) cooc
              $ concat
              $ map (\x -> listToDirectedCombiWith (\y -> getIdxInRoots y p) $ (Set.toList . getClique) x)
              $ (concat . elems) m
  where
    --------------------------------------
    fisNgrams :: [Ngrams]
    fisNgrams = foldl (\mem x -> union mem $ (Set.toList . getClique) x) [] $ (concat . elems) m
    --------------------------------------
    docs :: Double
    docs = fromIntegral $ foldl (\mem x -> mem + (getSupport x)) 0 $ (concat . elems) m
    --------------------------------------
    cooc :: Map (Int, Int) (Double)
    cooc = Map.fromList $ map (\x -> (x,0)) (listToDirectedCombiWith (\y -> getIdxInRoots y p) fisNgrams)
    --------------------------------------



-- | To transform a tuple of group's information into a coocurency Matrix
toCooc :: [([Int],Double)] -> Map (Int, Int) Double
toCooc l = map (/docs)
         $ foldl (\mem x -> adjust (+1) x mem) cooc
         $ concat
         $ map (\x -> listToFullCombi $ fst x) l
  where
    --------------------------------------
    idx :: [Int]
    idx = nub $ concat $ map fst l
    --------------------------------------
    docs :: Double
    docs = sum $ map snd l
    --------------------------------------
    cooc :: Map (Int, Int) (Double)
    cooc = Map.fromList $ map (\x -> (x,0)) $ listToFullCombi idx
    --------------------------------------    


-- | To reduce a coocurency Matrix to some keys
getSubCooc :: [Int] -> Map (Int, Int) Double -> Map (Int, Int) Double
getSubCooc idx cooc = filterWithKey (\k _ -> (elem (fst k) idx)
                                          && (elem (snd k) idx)) cooc


-- | To get a coocurency Matrix related to a given list of Periods
getCooc :: [PhyloPeriodId] -> Phylo -> Map (Int, Int) Double
getCooc prds p = toCooc $ map (\g -> (getGroupNgrams g,getGroupMeta "support" g)) gs
  where
    --------------------------------------
    -- | Here we need to go back to the level 1 (aka : the Fis level)
    gs :: [PhyloGroup]
    gs = filter (\g -> elem (getGroupPeriod g) prds ) $ getGroupsWithLevel 1 p
    -------------------------------------- 


-- phyloCooc :: Map (Int, Int) Double
-- phyloCooc = fisToCooc phyloFis phylo1_0_1
