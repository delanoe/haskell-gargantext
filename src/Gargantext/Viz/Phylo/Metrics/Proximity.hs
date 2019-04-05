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

module Gargantext.Viz.Phylo.Metrics.Proximity
  where

import Data.List        (null)
import Data.Map         (Map,elems,unionWith,intersectionWith,intersection,size)
import Gargantext.Prelude             hiding (head)

-- | To process the weightedLogJaccard between two PhyloGroup fields
weightedLogJaccard :: Double -> Map (Int, Int) Double -> Map (Int, Int) Double -> Double
weightedLogJaccard s f1 f2
  | null wUnion      = 0
  | wUnion == wInter = 1 
  | s == 0           = (fromIntegral $ length wInter)/(fromIntegral $ length wUnion)
  | s > 0            = (sumInvLog wInter)/(sumInvLog wUnion)
  | otherwise        = (sumLog wInter)/(sumLog wUnion)
  where 
    --------------------------------------
    wInter :: [Double]
    wInter = elems $ intersectionWith (+) f1 f2
    --------------------------------------
    wUnion :: [Double]
    wUnion = elems $ unionWith (+) f1 f2  
    --------------------------------------
    sumInvLog :: [Double] -> Double
    sumInvLog l = foldl (\mem x -> mem + (1 / log (s + x))) 0 l
    --------------------------------------
    sumLog :: [Double] -> Double
    sumLog l = foldl (\mem x -> mem + log (s + x)) 0 l  
    --------------------------------------  


-- | To process the Hamming distance between two PhyloGroup fields 
hamming :: Map (Int, Int) Double -> Map (Int, Int) Double -> Double
hamming f1 f2 = fromIntegral $ max ((size inter) - (size f1)) ((size inter) - (size f2))
  where
    --------------------------------------
    inter :: Map (Int, Int) Double
    inter = intersection f1 f2 
    --------------------------------------
