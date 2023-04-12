{-|
Module      : Gargantext.Core.Viz.Graph.Bridgeness
Description : Bridgeness filter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Let be a graph Bridgeness filters inter-communities links in two ways.
If the partitions are known, filtering is uniform to expose the communities clearly for the beginners.
But


uniformly
filters inter-communities links.

TODO use Map LouvainNodeId (Map LouvainNodeId)
-}

{-# LANGUAGE BangPatterns #-}

module Gargantext.Core.Viz.Graph.Bridgeness -- (bridgeness)
  where

import Data.Map.Strict (Map, fromListWith, lookup, toList, mapWithKey, elems)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import Data.Set (Set)
import Data.Tuple.Extra (swap)
import Debug.Trace (trace)
import Gargantext.Core.Methods.Similarities (Similarity(..))
import Gargantext.Prelude
import Prelude (pi)
import Graph.Types (ClusterNode(..))
import qualified Data.List        as List
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import qualified Data.Tuple.Extra as Tuple
import qualified Data.IntMap      as Dico
----------------------------------------------------------------------

type Partitions = Map (Int, Int) Double -> IO [ClusterNode]
type Partitions' = Map (Int, Int) Double -> IO [Set NodeId]
----------------------------------------------------------------------
nodeId2comId :: ClusterNode -> (NodeId, CommunityId)
nodeId2comId (ClusterNode i1 i2) = (i1, i2)

type NodeId        = Int
type CommunityId   = Int

----------------------------------------------------------------------
-- recursiveClustering : to get more granularity of a given clustering
-- tested with spinglass clustering only (WIP)
recursiveClustering' :: Partitions' -> Map (Int, Int) Double -> IO [[Set NodeId]]
recursiveClustering' f mp = do
  let
    n :: Double
    n = fromIntegral $ Set.size
      $ Set.unions  $ List.concat
      $ map (\(k1,k2) -> map Set.singleton [k1, k2])
      $ Map.keys mp

    t :: Int
    t = round $ 2 * n / sqrt n

  ss <- f mp
  mapM (\s -> if Set.size s > t then f (removeNodes s mp) else pure [s]) ss

----------------------------------------------------------------------
recursiveClustering :: Partitions -> Map (Int, Int) Double -> IO [ClusterNode]
recursiveClustering f mp = do
  let
    n :: Double
    n = fromIntegral $ Set.size
      $ Set.unions  $ List.concat
      $ map (\(k1,k2) -> map Set.singleton [k1, k2])
      $ Map.keys mp

    t :: Int
    t = round $ 2 * n / sqrt n

  (toSplit,others) <- List.span (\a -> Set.size a > t) <$> clusterNodes2sets <$> f mp
  cls' <- mapM f $ map (\s -> removeNodes s mp) toSplit
  pure $ setNodes2clusterNodes $ others <> (List.concat $ map clusterNodes2sets cls')


----------------------------------------------------------------------
setNodes2clusterNodes :: [Set NodeId] -> [ClusterNode]
setNodes2clusterNodes ns = List.concat $ map (\(n,ns') -> toCluster n ns') $ zip [1..] ns
  where
    toCluster :: CommunityId -> Set NodeId -> [ClusterNode]
    toCluster cId setNodeId = map (\n -> ClusterNode n cId) (Set.toList setNodeId)

clusterNodes2map :: [ClusterNode] -> Map NodeId Int
clusterNodes2map = Map.fromList . map (\(ClusterNode nId cId) -> (nId, cId))

removeNodes :: Set NodeId
            -> Map (NodeId, NodeId) Double
            -> Map (NodeId, NodeId) Double
removeNodes s = Map.filterWithKey (\(n1,n2) _v -> Set.member n1 s && Set.member n2 s)


clusterNodes2sets :: [ClusterNode] -> [Set NodeId]
clusterNodes2sets = Dico.elems
                  . Dico.fromListWith (<>)
                  . (map ((Tuple.second Set.singleton) . swap . nodeId2comId))

----------------------------------------------------------------------
data Bridgeness = Bridgeness_Basic { bridgeness_partitions :: [ClusterNode]
                                   , bridgeness_filter     :: Double
                                   }
                | Bridgeness_Advanced { bridgeness_similarity :: Similarity
                                      , bridgness_confluence  :: Confluence
                                      }
                | Bridgeness_Recursive { br_partitions :: [[Set NodeId]]
                                       , br_filter     :: Double
                                       , br_similarity :: Similarity
                                       }


type Confluence = Map (NodeId, NodeId) Double

-- Filter Links between the Clusters
-- Links: Map (NodeId, NodeId) Double
-- List of Clusters: [Set NodeId]
bridgeness :: Bridgeness
            -> Map (NodeId, NodeId) Double
            -> Map (NodeId, NodeId) Double
bridgeness (Bridgeness_Recursive sn f sim) m =
  Map.unions $ [linksBetween] <> map (\s -> bridgeness (Bridgeness_Basic (setNodes2clusterNodes s) (if sim == Conditional then pi*f else f)) m') sn
    where
      (linksBetween, m') = Map.partitionWithKey (\(n1,n2) _v -> Map.lookup n1 mapNodeIdClusterId
                                                             /= Map.lookup n2 mapNodeIdClusterId
                                                ) $ bridgeness (Bridgeness_Basic clusters f) m
      clusters = setNodes2clusterNodes (map Set.unions sn)
      mapNodeIdClusterId = clusterNodes2map clusters


bridgeness (Bridgeness_Advanced sim c) m = Map.fromList
                $ List.filter (\x -> if sim == Conditional then snd x > 0.2 else snd x > 0.02)
                $ map (\(ks, (v1,_v2)) -> (ks,v1))
                $ Map.toList
                $ Map.intersectionWithKey
                      (\k v1 v2 -> trace ("intersectionWithKey " <> (show (k, v1, v2))) (v1, v2)) m c


bridgeness (Bridgeness_Basic ns b) m = Map.fromList
                                     $ List.concat
                                     $ Map.elems
                                     $ filterComs (round b)
                                     $ groupEdges (Map.fromList $ map nodeId2comId ns) m


groupEdges :: (Ord comId, Ord nodeId)
           => Map nodeId comId
           -> Map (nodeId, nodeId) value
           -> Map (comId, comId) [((nodeId, nodeId), value)]
groupEdges m = fromListWith (<>)
             . catMaybes
             . map (\((n1,n2), d)
                     -> let
                          n1n2_m = (,) <$> lookup n1 m <*> lookup n2 m
                          n1n2_d = Just [((n1,n2),d)]
                        in (,) <$> n1n2_m <*> n1n2_d
                    )
             . toList

filterComs :: (Ord n1, Eq n2)
           => Int
           -> Map (n2, n2) [(a3, n1)]
           -> Map (n2, n2) [(a3, n1)]
filterComs b m = Map.filter (\n -> length n > 0) $ mapWithKey filter' m
  where
    filter' (c1,c2) a
      | c1 == c2  = a
      -- TODO use n here
      | otherwise = take (b * 2*n) $ List.sortOn (Down . snd) a
           where
            n :: Int
            n = round $ 100 * a' / t
            a'= fromIntegral $ length a
            t :: Double
            t = fromIntegral $ length $ List.concat $ elems m

--------------------------------------------------------------
-- Utils
{--
map2intMap :: Map (Int, Int) a -> IntMap (IntMap a)
map2intMap m = IntMap.fromListWith (<>)
             $ map (\((k1,k2), v) -> if k1 < k2
                                   then (k1, IntMap.singleton k2 v)
                                   else (k2, IntMap.singleton k1 v)
                    )
             $ Map.toList m

look :: (Int,Int) -> IntMap (IntMap a) -> Maybe a
look (k1,k2) m = if k1 < k2
                    then case (IntMap.lookup k1 m) of
                           Just m' -> IntMap.lookup k2 m'
                           _       -> Nothing
                    else look (k2,k1) m


{-
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

-}
