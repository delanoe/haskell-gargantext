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

import Data.List          (null,union,intersect)
import Data.Map           (Map,elems,unionWith,intersectionWith,intersection,size,filterWithKey)
import Gargantext.Prelude
-- import Debug.Trace (trace)

sumInvLog :: Double -> [Double] -> Double
sumInvLog s l = foldl (\mem x -> mem + (1 / log (s + x))) 0 l

sumLog :: Double -> [Double] -> Double
sumLog s l = foldl (\mem x -> mem + log (s + x)) 0 l  


-- -- | To process WeighedLogJaccard distance between to coocurency matrix
-- weightedLogJaccard :: Double -> Map (Int, Int) Double -> Map (Int, Int) Double -> Double -> Double
-- weightedLogJaccard sens cooc cooc' nbDocs
--   | null union'      = 0
--   | union' == inter' = 1
--   | sens == 0        = (fromIntegral $ length $ keysInter) / (fromIntegral $ length $ keysUnion)
--   | sens > 0         = (sumInvLog sens $ elems wInter) / (sumInvLog sens $ elems wUnion)
--   | otherwise        = (sumLog sens $ elems wInter) / (sumLog sens $ elems wUnion)
--   where
--     --------------------------------------
--     keysInter :: [Int]
--     keysInter = nub $ concat $ map (\(x,x') -> [x,x']) $ keys inter'
--     --------------------------------------
--     keysUnion :: [Int]
--     keysUnion = nub $ concat $ map (\(x,x') -> [x,x']) $ keys union'
--     --------------------------------------    
--     wInter :: Map (Int,Int) Double
--     wInter = map (/nbDocs) inter'
--     --------------------------------------
--     wUnion :: Map (Int,Int) Double
--     wUnion = map (/nbDocs) union'
--     --------------------------------------
--     inter' :: Map (Int, Int) Double
--     inter' = intersectionWith (+) cooc cooc'
--     --------------------------------------      
--     union' :: Map (Int, Int) Double
--     union' = unionWith (+) cooc cooc'
--     --------------------------------------


-- | To compute a jaccard similarity between two lists
jaccard :: [Int] -> [Int] -> Double
jaccard inter' union' = ((fromIntegral . length) $ inter') / ((fromIntegral . length) $ union')


-- | To get the diagonal of a matrix
toDiago :: Map (Int, Int) Double -> [Double]  
toDiago cooc = elems $ filterWithKey (\(x,x') _ -> x == x') cooc  


-- | To process WeighedLogJaccard distance between to coocurency matrix
weightedLogJaccard :: Double -> Double -> Map (Int, Int) Double -> Map (Int, Int) Double -> [Int] -> [Int] -> Double
weightedLogJaccard sens nbDocs cooc cooc' ngrams ngrams' 
  | null gInter      = 0
  | gInter == gUnion = 1
  | sens == 0        = jaccard gInter gUnion
  | sens > 0         = (sumInvLog sens wInter) / (sumInvLog sens wUnion)
  | otherwise        = (sumLog sens wInter) / (sumLog sens wUnion)
  where
    --------------------------------------
    gInter :: [Int] 
    gInter = intersect ngrams ngrams'   
    --------------------------------------
    gUnion :: [Int] 
    gUnion = union ngrams ngrams'
    --------------------------------------
    wInter :: [Double]
    wInter = toDiago $ map (/nbDocs) $ intersectionWith (+) cooc cooc'      
    --------------------------------------
    wUnion :: [Double]
    wUnion = toDiago $ map (/nbDocs) $ unionWith (+) cooc cooc'
    --------------------------------------



-- | To process the Hamming distance between two PhyloGroup fields 
hamming :: Map (Int, Int) Double -> Map (Int, Int) Double -> Double
hamming f1 f2 = fromIntegral $ max ((size inter) - (size f1)) ((size inter) - (size f2))
  where
    --------------------------------------
    inter :: Map (Int, Int) Double
    inter = intersection f1 f2 
    --------------------------------------
