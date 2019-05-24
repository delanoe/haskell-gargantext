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
import Data.Map         (Map,elems,unionWith,intersectionWith,intersection,size,keys)
import Gargantext.Prelude
-- import Debug.Trace (trace)

sumInvLog :: Double -> [Double] -> Double
sumInvLog s l = foldl (\mem x -> mem + (1 / log (s + x))) 0 l

sumLog :: Double -> [Double] -> Double
sumLog s l = foldl (\mem x -> mem + log (s + x)) 0 l  


-- | To process WeighedLogJaccard distance between to coocurency matrix
weightedLogJaccard :: Double -> Map (Int, Int) Double -> Map (Int, Int) Double -> Double -> Double
weightedLogJaccard sens cooc cooc' nbDocs
  | null union'      = 0
  | union' == inter' = 1
  | sens == 0        = (fromIntegral $ length $ keys inter') / (fromIntegral $ length $ keys union')
  | sens > 0         = (sumInvLog sens $ elems wInter) / (sumInvLog sens $ elems wUnion)
  | otherwise        = (sumLog sens $ elems wInter) / (sumLog sens $ elems wUnion)
  where
    --------------------------------------
    wInter :: Map (Int,Int) Double
    wInter = map (/nbDocs) inter'
    --------------------------------------
    wUnion :: Map (Int,Int) Double
    wUnion = map (/nbDocs) union'
    --------------------------------------
    inter' :: Map (Int, Int) Double
    inter' = intersectionWith (+) cooc cooc'
    --------------------------------------      
    union' :: Map (Int, Int) Double
    union' = unionWith (+) cooc cooc'
    --------------------------------------


-- | To process the Hamming distance between two PhyloGroup fields 
hamming :: Map (Int, Int) Double -> Map (Int, Int) Double -> Double
hamming f1 f2 = fromIntegral $ max ((size inter) - (size f1)) ((size inter) - (size f2))
  where
    --------------------------------------
    inter :: Map (Int, Int) Double
    inter = intersection f1 f2 
    --------------------------------------
