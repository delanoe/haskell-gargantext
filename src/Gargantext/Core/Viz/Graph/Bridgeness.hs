{-|
Module      : Gargantext.Core.Viz.Graph.Bridgeness
Description : Bridgeness filter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Let be a graph with partitions (from Louvain algo), Bridgeness uniformly
filters inter-communities links.

TODO rewrite Bridgeness with "equivalence structurale" metrics (Confluence)
TODO use Map LouvainNodeId (Map LouvainNodeId)
-}

{-# LANGUAGE BangPatterns #-}

module Gargantext.Core.Viz.Graph.Bridgeness -- (bridgeness)
  where

import Data.List (concat, sortOn)
import Data.Map (Map, fromListWith, lookup, toList, mapWithKey, elems)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Gargantext.Prelude
import Graph.Types (ClusterNode(..))
import Data.Ord (Down(..))
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

----------------------------------------------------------------------
type Partitions a = Map (Int, Int) Double -> IO [a]
----------------------------------------------------------------------
class ToComId a where
  nodeId2comId :: a -> (NodeId,CommunityId)

type NodeId        = Int
type CommunityId   = Int

----------------------------------------------------------------------
instance ToComId ClusterNode where
  nodeId2comId (ClusterNode i1 i2) = (i1, i2)

----------------------------------------------------------------------
----------------------------------------------------------------------
type Bridgeness = Double
type Confluence = Map (NodeId, NodeId) Double

bridgeness3 :: Confluence
           -> Map (NodeId, NodeId) Double
           -> Map (NodeId, NodeId) Double
bridgeness3 _ m = m


bridgeness2 :: Confluence
           -> Map (NodeId, NodeId) Double
           -> Map (NodeId, NodeId) Double
bridgeness2 c m = Map.fromList
                $ List.filter (\((k1,k2),_v) -> if k1 > k2
                                                   then fromMaybe False (Set.member k2 <$> Map.lookup k1 toKeep)
                                                   else fromMaybe False (Set.member k1 <$> Map.lookup k2 toKeep)
                               )
                $ m'
  where
    toKeep :: Map NodeId (Set NodeId)
    !toKeep = Map.fromListWith (<>)
            $ map (\((k1,k2), _v) -> if k1 > k2 
                                        then (k1, Set.singleton k2)
                                        else (k2, Set.singleton k1)
                  )
            $ List.take n
            $ List.sortOn (Down . snd)
            $ Map.toList c

    !m' = Map.toList m
    n :: Int
    !n = round $ (fromIntegral $ List.length m') / (2 :: Double)
    
{-
    n :: Int
    n = Set.size $ Set.fromList $ as <> bs
      where
        (as, bs) = List.unzip $ Map.keys m
-}

bridgeness :: ToComId a
           => Confluence
           -> [a]
           -> Map (NodeId, NodeId) Double
           -> Map (NodeId, NodeId) Double
bridgeness = bridgenessWith nodeId2comId
  where
    bridgenessWith :: (a -> (Int, Int))
               -> Confluence
               -> [a]
               -> Map (Int, Int) Double
               -> Map (Int, Int) Double
    bridgenessWith f b ns = Map.fromList
                           . concat
                           . Map.elems
                           . filterComs b
                           . groupEdges (Map.fromList $ map f ns)


groupEdges :: (Ord a, Ord b1)
           => Map b1 a
           -> Map (b1, b1) b2
           -> Map (a, a) [((b1, b1), b2)]
groupEdges m = fromListWith (<>)
             . catMaybes
             . map (\((n1,n2), d)
                     -> let
                          n1n2_m = (,) <$> lookup n1 m <*> lookup n2 m
                          n1n2_d = Just [((n1,n2),d)]
                        in (,) <$> n1n2_m <*> n1n2_d
                    )
             . toList

-- | TODO : sortOn Confluence
filterComs :: (Ord n1, Eq n2)
           => p
           -> Map (n2, n2) [(a3, n1)]
           -> Map (n2, n2) [(a3, n1)]
filterComs _b m = Map.filter (\n -> length n > 0) $ mapWithKey filter' m
  where
    filter' (c1,c2) a
      | c1 == c2  = a
      -- TODO use n here
      | otherwise = take 1 $ sortOn (Down . snd) a
           where
            _n :: Int
            _n = round $ 100 * a' / t
            a'= fromIntegral $ length a
            t :: Double
            t = fromIntegral $ length $ concat $ elems m

--------------------------------------------------------------

{--
Compute the median of a list
From:  https://hackage.haskell.org/package/dsp-0.2.5.1/docs/src/Numeric.Statistics.Median.html
Compute the center of the list in a more lazy manner
and thus halves memory requirement.
-}
median :: (Ord a, Fractional a) => [a] -> a
median [] = panic "medianFast: empty list has no median"
median zs =
   let recurse (x0:_)    (_:[])   = x0
       recurse (x0:x1:_) (_:_:[]) = (x0+x1)/2
       recurse (_:xs)    (_:_:ys) = recurse xs ys
       recurse _ _  =
          panic "median: this error cannot occur in the way 'recurse' is called"
   in  recurse zs zs

