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

import Data.List                    (union,concat,nub,sort)
import Data.Map                     (Map,elems,adjust,filterWithKey,fromListWith,fromList,restrictKeys)
import Data.Set                     (Set)
import Data.Vector                  (Vector)
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





-- | To transform a list of index into a cooc matrix 
listToCooc :: [Int] -> Map (Int,Int) Double
listToCooc lst = fromList $ map (\combi -> (combi,1)) $ listToFullCombi lst


-- | To transform a list of ngrams into a list of indexes
ngramsToIdx :: [Ngrams] -> Vector Ngrams -> [Int]
ngramsToIdx ns v = sort $ map (\n -> getIdxInVector n v) ns


-- | To build the cooc matrix by years out of the corpus
docsToCooc :: [Document] -> Vector Ngrams -> Map Date (Map (Int,Int) Double)
docsToCooc docs fdt = fromListWith sumCooc 
                    $ map (\(d,l) -> (d, listToCooc l))
                    $ map (\doc -> (date doc, ngramsToIdx (text doc) fdt)) docs  


-- | To sum all the docs produced during a list of years 
sumDocsByYears :: Set Date -> Map Date Double -> Double
sumDocsByYears years m = sum $ elems $ restrictKeys m years    


-- | To get the cooc matrix of a group
groupToCooc :: PhyloGroup -> Phylo -> Map (Int,Int) Double
groupToCooc g p = getMiniCooc (listToFullCombi $ getGroupNgrams g) (periodsToYears [getGroupPeriod g]) (getPhyloCooc p)


-- | To get the union of the cooc matrix of two groups
unionOfCooc :: PhyloGroup -> PhyloGroup -> Phylo -> Map (Int,Int) Double
unionOfCooc g g' p = sumCooc (groupToCooc g p) (groupToCooc g' p)  




-- phyloCooc :: Map (Int, Int) Double
-- phyloCooc = fisToCooc phyloFis phylo1_0_1
