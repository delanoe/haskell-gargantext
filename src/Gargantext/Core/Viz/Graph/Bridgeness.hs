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

import Gargantext.Core.Methods.Similarities (Similarity(..))
-- import Data.IntMap (IntMap)
import Data.Map (Map, fromListWith, lookup, toList, mapWithKey, elems)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import Debug.Trace (trace)
import Gargantext.Prelude
import Graph.Types (ClusterNode(..))
-- import qualified Data.IntMap as IntMap
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set

----------------------------------------------------------------------

type Partitions = Map (Int, Int) Double -> IO [ClusterNode]
----------------------------------------------------------------------
nodeId2comId :: ClusterNode -> (NodeId, CommunityId)
nodeId2comId (ClusterNode i1 i2) = (i1, i2)

type NodeId        = Int
type CommunityId   = Int

----------------------------------------------------------------------
----------------------------------------------------------------------
data Bridgeness = Bridgeness_Basic { bridgeness_partitions :: [ClusterNode]
                                   , bridgeness_filter     :: Double
                                   }
                | Bridgeness_Advanced { bridgeness_similarity :: Similarity
                                      , bridgness_confluence  :: Confluence
                                      }

type Confluence = Map (NodeId, NodeId) Double


bridgeness :: Bridgeness
            -> Map (NodeId, NodeId) Double
            -> Map (NodeId, NodeId) Double
bridgeness (Bridgeness_Advanced sim c) m = Map.fromList
                $ map (\(ks, (v1,_v2)) -> (ks,v1))
                $ List.take (if sim == Conditional then 2*n else 3*n)
                $ List.sortOn (Down . (snd . snd))
                $ Map.toList
                $ trace ("bridgeness3 m c" <> show (m,c)) $ Map.intersectionWithKey (\k v1 v2 -> trace ("intersectionWithKey " <> (show (k, v1, v2))) (v1, v2)) m c
  where
    !m' = Map.toList m
    n :: Int
    !n = trace ("bridgeness m size: " <> (show $ List.length m'))
       $ round
       $ (fromIntegral $ List.length m') / (log $ fromIntegral nodesNumber :: Double)

    nodesNumber :: Int
    nodesNumber = Set.size $ Set.fromList $ as <> bs
      where
        (as, bs) = List.unzip $ Map.keys m

bridgeness (Bridgeness_Basic ns b) m = Map.fromList
                                     $ List.concat
                                     $ Map.elems
                                     $ filterComs b
                                     $ groupEdges (Map.fromList $ map nodeId2comId ns) m

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
      | otherwise = take 1 $ List.sortOn (Down . snd) a
           where
            _n :: Int
            _n = round $ 100 * a' / t
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
