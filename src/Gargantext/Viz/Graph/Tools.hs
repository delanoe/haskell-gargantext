{-|
Module      : Gargantext.Viz.Graph.Tools
Description : Tools to build Graph
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}

module Gargantext.Viz.Graph.Tools
  where

import Debug.Trace (trace)
import Data.Graph.Clustering.Louvain.CplusPlus (LouvainNode(..))
import Data.Graph.Clustering.Louvain.CplusPlus (cLouvain)
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Core.Statistics
import Gargantext.Viz.Graph
import Gargantext.Viz.Graph.Bridgeness (bridgeness)
import Gargantext.Viz.Graph.Distances.Matrice (measureConditional)
import Gargantext.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map)
import Gargantext.Viz.Graph.IGraph (mkGraphUfromEdges)
import Gargantext.Viz.Graph.Proxemy (confluence)
import GHC.Float (sin, cos)
import qualified IGraph as Igraph
import qualified IGraph.Algorithms.Layout as Layout
import qualified Data.Vector.Storable as Vec
import qualified Data.Map  as Map
import qualified Data.List as List

type Threshold = Double

cooc2graph :: Threshold
           -> (Map (Text, Text) Int)
           -> IO Graph
cooc2graph threshold myCooc = do
  let (ti, _) = createIndices myCooc
      myCooc' = toIndex ti myCooc
      matCooc = map2mat 0 (Map.size ti) $ Map.filter (> 1) myCooc'
      distanceMat = measureConditional matCooc
      distanceMap = Map.filter (> threshold) $ mat2map distanceMat

  let nodesApprox :: Int
      nodesApprox = n'
        where
          (as, bs) = List.unzip $ Map.keys distanceMap
          n' = Set.size $ Set.fromList $ as <> bs
      ClustersParams rivers level = trace ("nodesApprox: " <> show nodesApprox) $ clustersParams nodesApprox


  partitions <- case Map.size distanceMap > 0 of
    True  -> trace ("level" <> show level) $ cLouvain level distanceMap
    False -> panic "Text.Flow: DistanceMap is empty"

  let bridgeness' = trace ("rivers: " <> show rivers) $ bridgeness rivers partitions distanceMap
  let confluence' = confluence (Map.keys bridgeness') 3 True False

  data2graph (Map.toList ti) myCooc' bridgeness' confluence' partitions



data ClustersParams = ClustersParams { bridgness :: Double
                                     , louvain   :: Text
                                     } deriving (Show)

clustersParams :: Int -> ClustersParams
clustersParams x = ClustersParams (fromIntegral x) y
  where
    y | x < 100  = "0.01"
      | x < 350  = "0.01"
      | x < 500  = "0.01"
      | x < 1000 = "0.1"
      | otherwise = "1"


----------------------------------------------------------
-- | From data to Graph
data2graph :: [(Text, Int)]
           -> Map (Int, Int) Int
           -> Map (Int, Int) Double
           -> Map (Int, Int) Double
           -> [LouvainNode]
           -> IO Graph
data2graph labels coocs bridge conf partitions = do
    
    let community_id_by_node_id = Map.fromList [ (n, c) | LouvainNode n c <- partitions ]

    nodes <- mapM (setCoord ForceAtlas labels bridge)
          [ (n, Node { node_size = maybe 0 identity (Map.lookup (n,n) coocs)
                   , node_type = Terms -- or Unknown
                   , node_id    = cs (show n)
                   , node_label = l
                   , node_x_coord = 0
                   , node_y_coord = 0
                   , node_attributes =
                     Attributes { clust_default = maybe 0 identity
                                (Map.lookup n community_id_by_node_id) } }
               )
            | (l, n) <- labels
            , Set.member n $ Set.fromList
                           $ List.concat
                           $ map (\((s,t),d) -> if d > 0 && s /=t then [s,t] else [])
                           $ Map.toList bridge
            ]

    let edges = [ Edge { edge_source = cs (show s)
                       , edge_target = cs (show t)
                       , edge_weight =  d
                       , edge_confluence = maybe 0 identity $ Map.lookup (s,t) conf
                   -- , edge_confluence = maybe (panic "E: data2graph edges") identity $ Map.lookup (s,t) conf
                   , edge_id     = cs (show i) }
                   | (i, ((s,t), d)) <- zip ([0..]::[Integer]) (Map.toList bridge), s /= t, d > 0
                   ]

    pure $ Graph nodes edges Nothing

------------------------------------------------------------------------

data Layout = KamadaKawai | ACP | ForceAtlas


setCoord' :: (Int -> (Double, Double)) -> (Int, Node) -> Node
setCoord' f (i,n) = n { node_x_coord = x, node_y_coord = y }
  where
    (x,y) = f i


-- | ACP
setCoord :: Ord a => Layout -> [(a, Int)] -> Map (Int, Int) Double -> (Int, Node) -> IO Node
setCoord l labels m (n,node) = getCoord l labels m n
                           >>= \(x,y) -> pure $ node { node_x_coord = x
                                                     , node_y_coord = y
                                                     }


getCoord :: Ord a => Layout
                  -> [(a, Int)] -> Map (Int, Int) Double -> Int -> IO (Double, Double)
getCoord KamadaKawai _ m n = layout m n

getCoord ForceAtlas _ _ n = pure (sin d, cos d)
  where
    d = fromIntegral n

getCoord ACP labels m n = pure $ to2d $ maybe (panic "Graph.Tools no coordinate") identity
             $ Map.lookup n
             $ pcaReduceTo (Dimension 2)
             $ mapArray labels m
  where
    to2d :: Vec.Vector Double -> (Double, Double)
    to2d v  = (x',y')
      where
        ds = take 2 $ Vec.toList v
        x'  = head' "to2d" ds
        y'  = last' "to2d" ds

    mapArray :: Ord a => [(a, Int)] -> Map (Int, Int) Double -> Map Int (Vec.Vector Double)
    mapArray items m' = Map.fromList [ toVec n' ns m' | n' <- ns ]
      where
        ns = map snd items

    toVec :: Int -> [Int] -> Map (Int,Int) Double -> (Int, Vec.Vector Double)
    toVec n' ns' m' = (n', Vec.fromList $ map (\n'' -> maybe 0 identity $ Map.lookup (n',n'') m') ns')
------------------------------------------------------------------------

-- | KamadaKawai Layout
-- TODO TEST: check labels, nodeId and coordinates
layout :: Map (Int, Int) Double -> Int -> IO (Double, Double)
layout m n = maybe (panic "") identity <$> Map.lookup n <$> coord
  where
    coord :: IO (Map Int (Double,Double))
    coord = Map.fromList <$> List.zip (Igraph.nodes g) <$> (Layout.getLayout g p)
    --p = Layout.defaultLGL
    p = Layout.defaultKamadaKawai
    g = mkGraphUfromEdges $ map fst $ List.filter (\e -> snd e > 0) $ Map.toList m

