{-|
Module      : Gargantext.Core.Viz.Graph.Tools
Description : Tools to build Graph
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Viz.Graph.Tools
  where

-- import Data.Graph.Clustering.Louvain (hLouvain, {-iLouvainMap-})
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Text (Text)
import Debug.Trace (trace)
import GHC.Float (sin, cos)
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Core.Methods.Distances (Distance(..), measure)
import Gargantext.Core.Methods.Graph.BAC.Proxemy (confluence)
import Gargantext.Core.Statistics
import Gargantext.Core.Viz.Graph
import Gargantext.Core.Viz.Graph.Bridgeness (bridgeness, Partitions, ToComId(..))
import Gargantext.Core.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map, Index, MatrixShape(..))
import Gargantext.Core.Viz.Graph.Tools.IGraph (mkGraphUfromEdges, spinglass)
import Gargantext.Prelude
import Graph.Types (ClusterNode)
import IGraph.Random -- (Gen(..))
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Vector.Storable     as Vec
import qualified IGraph                   as Igraph
import qualified IGraph.Algorithms.Layout as Layout


-------------------------------------------------------------

defaultClustering :: Map (Int, Int) Double -> IO [ClusterNode]
defaultClustering = spinglass 1

-------------------------------------------------------------

type Threshold = Double

cooc2graph' :: Ord t => Distance
                     -> Double
                     -> Map (t, t) Int
                     -> Map (Index, Index) Double
cooc2graph' distance threshold myCooc
    = Map.filter (> threshold)
    $ mat2map
    $ measure distance
    $ case distance of
        Conditional    -> map2mat Triangle 0 tiSize
        Distributional -> map2mat Square   0 tiSize
    $ Map.filter (> 1) myCooc'

     where
        (ti, _) = createIndices myCooc
        tiSize  = Map.size ti
        myCooc' = toIndex ti myCooc


data PartitionMethod = Louvain | Spinglass

-- | coocurrences graph computation
cooc2graphWith :: PartitionMethod
               -> Distance
               -> Threshold
               -> HashMap (NgramsTerm, NgramsTerm) Int
               -> IO Graph
cooc2graphWith Louvain   = undefined -- TODO use IGraph bindings
cooc2graphWith Spinglass = cooc2graphWith' (spinglass 1)

cooc2graph'' :: Ord t => Distance
                      -> Double
                      -> Map (t, t) Int
                      -> Map (Index, Index) Double
cooc2graph'' distance threshold myCooc = neighbourMap
  where
    (ti, _) = createIndices myCooc
    myCooc' = toIndex ti myCooc
    matCooc = map2mat Triangle 0 (Map.size ti) $ Map.filter (> 1) myCooc'
    distanceMat = measure distance matCooc
    neighbourMap = filterByNeighbours threshold
                 $ mat2map distanceMat


-- Quentin
filterByNeighbours :: Double -> Map (Index, Index) Double -> Map (Index, Index) Double
filterByNeighbours threshold distanceMap = filteredMap
  where 
    indexes :: [Index]
    indexes = List.nub $ List.concat $ map (\(idx,idx') -> [idx,idx'] ) $ Map.keys distanceMap
    filteredMap :: Map (Index, Index) Double
    filteredMap = Map.fromList
                $ List.concat 
                $ map (\idx -> 
                          let selected = List.reverse
                                       $ List.sortOn snd
                                       $ Map.toList 
                                       $ Map.filter (> 0)
                                       $ Map.filterWithKey (\(from,_) _ -> idx == from) distanceMap
                           in List.take (round threshold) selected
                      ) indexes                 


doDistanceMap :: Distance
               -> Threshold
               -> HashMap (NgramsTerm, NgramsTerm) Int
               -> (Map (Int,Int) Double, Map (Index, Index) Int, Map NgramsTerm Index)
doDistanceMap distance threshold myCooc = (distanceMap, myCooc', ti)
  where
    -- TODO remove below
    theMatrix = Map.fromList
              $ HashMap.toList myCooc

    (ti, _) = createIndices theMatrix
    tiSize  = Map.size ti
    myCooc' = toIndex ti theMatrix
    matCooc = case distance of  -- Shape of the Matrix
                Conditional    -> map2mat Triangle 0 tiSize
                Distributional -> map2mat Square     0 tiSize
            $ case distance of   -- Removing the Diagonal ?
                Conditional     -> Map.filterWithKey (\(a,b) _ -> a /= b)
                Distributional  -> identity
            $ Map.filter (>1) myCooc'

    similarities = measure distance matCooc
    links = round (let n :: Double = fromIntegral tiSize in n * log n)

    distanceMap = Map.fromList  $ List.take links
                $ List.sortOn snd
                $ Map.toList
                $ case distance of
                    Conditional    -> Map.filter (> threshold)
                    Distributional -> Map.filter (> 0)
                $ mat2map similarities

cooc2graphWith' :: ToComId a
               => Partitions a
               -> Distance
               -> Threshold
               -> HashMap (NgramsTerm, NgramsTerm) Int
               -> IO Graph
cooc2graphWith' doPartitions distance threshold myCooc = do
  let
    (distanceMap, myCooc', ti) = doDistanceMap distance threshold myCooc

    nodesApprox :: Int
    nodesApprox = n'
      where
        (as, bs) = List.unzip $ Map.keys distanceMap
        n' = Set.size $ Set.fromList $ as <> bs
    ClustersParams rivers _level = clustersParams nodesApprox

{- -- Debug
  saveAsFileDebug "debug/distanceMap" distanceMap
  printDebug "similarities" similarities
-}

  partitions <- if (Map.size distanceMap > 0)
      then doPartitions distanceMap
      else panic "Text.Flow: DistanceMap is empty"

  let
    -- bridgeness' = distanceMap
    bridgeness' = trace ("Rivers: " <> show rivers)
                $ bridgeness rivers partitions distanceMap

    confluence' = confluence (Map.keys bridgeness') 3 True False

  pure $ data2graph (Map.toList $ Map.mapKeys unNgramsTerm ti)
                    myCooc' bridgeness' confluence' partitions


------------------------------------------------------------------------
------------------------------------------------------------------------
data ClustersParams = ClustersParams { bridgness :: Double
                                     , louvain   :: Text
                                     } deriving (Show)

clustersParams :: Int -> ClustersParams
clustersParams x = ClustersParams (fromIntegral x) "0.00000001" -- y
  {- where
    y | x < 100  = "0.000001"
      | x < 350  = "0.000001"
      | x < 500  = "0.000001"
      | x < 1000 = "0.000001"
      | otherwise = "1"
 -}

----------------------------------------------------------
-- | From data to Graph
data2graph :: ToComId a 
           => [(Text, Int)]
           -> Map (Int, Int) Int
           -> Map (Int, Int) Double
           -> Map (Int, Int) Double
           -> [a]
           -> Graph
data2graph labels coocs bridge conf partitions = Graph nodes edges Nothing
  where

    community_id_by_node_id = Map.fromList $ map nodeId2comId partitions

    nodes = map (setCoord ForceAtlas labels bridge)
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

    edges = [ Edge { edge_source = cs (show s)
                       , edge_target = cs (show t)
                       , edge_weight =  d
                       , edge_confluence = maybe 0 identity $ Map.lookup (s,t) conf
                   -- , edge_confluence = maybe (panic "E: data2graph edges") identity $ Map.lookup (s,t) conf
                       , edge_id     = cs (show i)
                   }
                   | (i, ((s,t), d)) <- zip ([0..]::[Integer] )
                                            (Map.toList bridge)
                   , s /= t, d > 0
                   ]


------------------------------------------------------------------------

data Layout = KamadaKawai | ACP | ForceAtlas


setCoord' :: (Int -> (Double, Double)) -> (Int, Node) -> Node
setCoord' f (i,n) = n { node_x_coord = x, node_y_coord = y }
  where
    (x,y) = f i


-- | ACP
setCoord :: Ord a => Layout -> [(a, Int)] -> Map (Int, Int) Double -> (Int, Node) -> Node
setCoord l labels m (n,node) = node { node_x_coord = x
                                    , node_y_coord = y
                                    }
  where
    (x,y) = getCoord l labels m n


getCoord :: Ord a
         => Layout
         -> [(a, Int)]
         -> Map (Int, Int) Double
         -> Int
         -> (Double, Double)
getCoord KamadaKawai _ _m _n = undefined -- layout m n

getCoord ForceAtlas _ _ n = (sin d, cos d)
  where
    d = fromIntegral n

getCoord ACP labels m n = to2d $ maybe (panic "Graph.Tools no coordinate") identity
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
layout :: Map (Int, Int) Double -> Int -> Gen -> (Double, Double)
layout m n gen = maybe (panic "") identity $ Map.lookup n $ coord
  where
    coord :: (Map Int (Double,Double))
    coord = Map.fromList $ List.zip (Igraph.nodes g) $ (Layout.layout g p gen)
    --p = Layout.defaultLGL
    p = Layout.kamadaKawai
    g = mkGraphUfromEdges $ map fst $ List.filter (\e -> snd e > 0) $ Map.toList m
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Debug
{-
-- measure logDistributional
dataDebug = map2mat Square (0::Int) 19 dataBug'

dataBug' :: Map (Int, Int) Int
dataBug' = Map.fromList [((0,0),28),((0,1),8),((0,2),6),((0,3),2),((0,5),4),((0,6),4),((0,7),2),((0,9),7),((0,10),4),((0,13),4),((0,14),2),((0,15),5),((0,16),8),((0,17),3),((1,1),28),((1,2),6),((1,3),7),((1,4),5),((1,5),7),((1,6),5),((1,7),2),((1,9),6),((1,10),7),((1,11),5),((1,13),6),((1,15),6),((1,16),14),((1,18),4),((2,2),39),((2,3),5),((2,4),4),((2,5),3),((2,6),4),((2,7),4),((2,8),3),((2,9),17),((2,10),4),((2,11),8),((2,12),2),((2,13),15),((2,14),4),((2,15),5),((2,16),21),((2,18),4),((3,3),48),((3,4),10),((3,5),7),((3,6),3),((3,7),7),((3,8),6),((3,9),12),((3,10),9),((3,11),8),((3,12),5),((3,13),15),((3,14),5),((3,15),9),((3,16),17),((3,18),4),((4,4),33),((4,5),2),((4,6),5),((4,7),7),((4,8),4),((4,9),6),((4,10),12),((4,11),8),((4,12),3),((4,13),16),((4,14),4),((4,15),4),((4,16),5),((4,17),2),((4,18),12),((5,5),27),((5,6),2),((5,8),3),((5,9),12),((5,10),6),((5,11),9),((5,13),4),((5,14),2),((5,15),7),((5,16),11),((5,18),4),((6,6),34),((6,7),4),((6,8),3),((6,9),12),((6,10),8),((6,11),2),((6,12),5),((6,13),6),((6,14),6),((6,15),5),((6,16),22),((6,17),8),((6,18),4),((7,7),27),((7,8),2),((7,9),6),((7,10),2),((7,11),4),((7,13),13),((7,15),2),((7,16),8),((7,17),6),((7,18),4),((8,8),30),((8,9),9),((8,10),6),((8,11),9),((8,12),6),((8,13),3),((8,14),3),((8,15),4),((8,16),15),((8,17),3),((8,18),5),((9,9),69),((9,10),9),((9,11),22),((9,12),15),((9,13),18),((9,14),10),((9,15),14),((9,16),48),((9,17),6),((9,18),9),((10,10),39),((10,11),15),((10,12),5),((10,13),11),((10,14),2),((10,15),4),((10,16),19),((10,17),3),((10,18),11),((11,11),48),((11,12),9),((11,13),20),((11,14),2),((11,15),13),((11,16),29),((11,18),13),((12,12),30),((12,13),4),((12,15),5),((12,16),16),((12,17),6),((12,18),2),((13,13),65),((13,14),10),((13,15),14),((13,16),23),((13,17),6),((13,18),10),((14,14),25),((14,16),9),((14,17),3),((14,18),3),((15,15),38),((15,16),17),((15,18),4),((16,16),99),((16,17),11),((16,18),14),((17,17),29),((18,18),23)]
-}
