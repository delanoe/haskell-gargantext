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

import Debug.Trace

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Swagger hiding (items)
import GHC.Float (sin, cos)
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Core.Methods.Distances (Distance(..), measure)
import Gargantext.Core.Methods.Distances.Conditional (conditional)
-- import Gargantext.Core.Methods.Graph.BAC.Proxemy (confluence)
import Gargantext.Core.Statistics
import Gargantext.Core.Viz.Graph
import Gargantext.Core.Viz.Graph.Bridgeness (bridgeness, Partitions, ToComId(..))
import Gargantext.Core.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map, Index, MatrixShape(..))
import Gargantext.Core.Viz.Graph.Tools.IGraph (mkGraphUfromEdges, spinglass)
import Gargantext.Core.Viz.Graph.Tools.Infomap (infomap)
import Gargantext.Core.Viz.Graph.Utils (edgesFilter)
import Gargantext.Prelude
import Graph.Types (ClusterNode)
import IGraph.Random -- (Gen(..))
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified Data.Vector.Storable     as Vec
import qualified Graph.BAC.ProxemyOptim   as BAC
import qualified IGraph                   as Igraph
import qualified IGraph.Algorithms.Layout as Layout


data PartitionMethod = Spinglass | Confluence | Infomap
    deriving (Generic, Eq, Ord, Enum, Bounded, Show)
instance FromJSON  PartitionMethod
instance ToJSON    PartitionMethod
instance ToSchema  PartitionMethod
instance Arbitrary PartitionMethod where
  arbitrary = elements [ minBound .. maxBound ]


-------------------------------------------------------------
defaultClustering :: Map (Int, Int) Double -> IO [ClusterNode]
-- defaultClustering x = pure $ BAC.defaultClustering x
defaultClustering x = spinglass 1 x

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



-- coocurrences graph computation
cooc2graphWith :: PartitionMethod
               -> Distance
               -> Threshold
               -> HashMap (NgramsTerm, NgramsTerm) Int
               -> IO Graph
cooc2graphWith Spinglass = cooc2graphWith' (spinglass 1)
cooc2graphWith Confluence= cooc2graphWith' (\x -> pure $ BAC.defaultClustering x)
cooc2graphWith Infomap   = cooc2graphWith' (infomap "--silent --two-level -N2")
                        -- TODO: change these options, or make them configurable in UI?


cooc2graphWith' :: ToComId a
               => Partitions a
               -> Distance
               -> Threshold
               -> HashMap (NgramsTerm, NgramsTerm) Int
               -> IO Graph
cooc2graphWith' doPartitions distance threshold myCooc = do
  let (distanceMap, diag, ti) = doDistanceMap distance threshold myCooc
  distanceMap `seq` trace "distanceMap OK" diag `seq` trace "diag OK" ti `seq` printDebug "ti done" ()

--{- -- Debug
  -- saveAsFileDebug "/tmp/distanceMap" distanceMap
  -- saveAsFileDebug "/tmp/distanceMap.keys" (List.length $ Map.keys distanceMap)
  -- printDebug "similarities" similarities
--}

  partitions <- if (Map.size distanceMap > 0)
      then doPartitions distanceMap
      else panic $ Text.unlines [ "[Gargantext.C.V.Graph.Tools] Similarity Matrix is empty"
                                , "Maybe you should add more Map Terms in your list"
                                , "Tutorial: link todo"
                                ]
  partitions `seq` printDebug "partitions done" ()
  let
    nodesApprox :: Int
    nodesApprox = n'
      where
        (as, bs) = List.unzip $ Map.keys distanceMap
        n' = Set.size $ Set.fromList $ as <> bs
    bridgeness' = bridgeness (fromIntegral nodesApprox) partitions distanceMap
    confluence' = Map.empty -- confluence (Map.keys bridgeness') 3 True False
  seq bridgeness' $ printDebug "bridgeness OK" ()
  saveAsFileDebug "/tmp/bridgeness" bridgeness'
  --seq confluence' $ printDebug "confluence OK" ()
  --saveAsFileDebug "/tmp/confluence" confluence'
  let g = data2graph ti diag bridgeness' confluence' partitions
  saveAsFileDebug "/tmp/graph" g
  pure g

doDistanceMap :: Distance
              -> Threshold
              -> HashMap (NgramsTerm, NgramsTerm) Int
              -> ( Map (Int,Int) Double
                 , Map (Index, Index) Int
                 , Map NgramsTerm Index
                 )
doDistanceMap Distributional threshold myCooc = (distanceMap, toIndex ti diag, ti)
  where
    -- TODO remove below
    (diag, theMatrix) = Map.partitionWithKey (\(x,y) _ -> x == y)
                      $ Map.fromList
                      $ HashMap.toList myCooc

    (ti, _it) = createIndices theMatrix
    tiSize  = Map.size ti

    similarities = (\m -> m `seq` trace "measure done" m)
                 $ (\m -> m `seq` trace "map2mat done" (measure Distributional m))
                 $ (\m -> m `seq` trace "toIndex done" (map2mat Square 0 tiSize m))
                 $ theMatrix `seq` trace "theMatrix done" (toIndex ti theMatrix)

    links = round (let n :: Double = fromIntegral tiSize in n * (log n)^(2::Int))

    distanceMap = Map.fromList . trace "fromList" identity
                $ List.take links
                $ List.reverse
                $ List.sortOn snd
                $ Map.toList
                $ edgesFilter
                $ (\m -> m `seq` trace "map2map done" (Map.filter (> threshold) m))
                $ similarities `seq` mat2map (trace "similarities done" similarities)

doDistanceMap Conditional threshold myCooc = (distanceMap, toIndex ti myCooc', ti)
  where
    myCooc' = Map.fromList $ HashMap.toList myCooc
    (ti, _it) = createIndices myCooc'

    links = round (let n :: Double = fromIntegral (Map.size ti) in n * log n)

    distanceMap = toIndex ti
                $ Map.fromList
                $ List.take links
                $ List.reverse
                $ List.sortOn snd
                $ HashMap.toList
                $ HashMap.filter (> threshold)
                $ conditional myCooc

----------------------------------------------------------
-- | From data to Graph

type Occurrences      = Int

data2graph :: ToComId a 
           => Map NgramsTerm Int
           -> Map (Int, Int) Occurrences
           -> Map (Int, Int) Double
           -> Map (Int, Int) Double
           -> [a]
           -> Graph
data2graph labels' occurences bridge conf partitions = Graph { _graph_nodes = nodes
                                                             , _graph_edges = edges
                                                             , _graph_metadata = Nothing
                                                             }
  where

    nodes = map (setCoord ForceAtlas labels bridge)
          [ (n, Node { node_size    = maybe 0 identity (Map.lookup (n,n) occurences)
                     , node_type    = Terms -- or Unknown
                     , node_id      = cs (show n)
                     , node_label   = unNgramsTerm l
                     , node_x_coord = 0
                     , node_y_coord = 0
                     , node_attributes = Attributes { clust_default = fromMaybe 0
                                                       (Map.lookup n community_id_by_node_id)
                                                    }
                     , node_children = []
                     }
               )
            | (l, n) <- labels
            , Set.member n nodesWithScores
            ]

    edges = [ Edge { edge_source = cs (show s)
                   , edge_target = cs (show t)
                   , edge_weight = weight
                   , edge_confluence = maybe 0 identity $ Map.lookup (s,t) conf
                   , edge_id     = cs (show i)
                   }
            | (i, ((s,t), weight)) <- zip ([0..]::[Integer] ) $ Map.toList bridge
            , s /= t
            , weight > 0
            ]

    community_id_by_node_id = Map.fromList
                            $ map nodeId2comId partitions

    labels = Map.toList labels'

    nodesWithScores = Set.fromList
                     $ List.concat
                     $ map (\((s,t),d) -> if d > 0 && s/=t then [s,t] else [])
                     $ Map.toList bridge


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
    toVec n' ns' m'' = (n', Vec.fromList $ map (\n'' -> maybe 0 identity $ Map.lookup (n',n'') m'') ns')
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
-- MISC Tools
cooc2graph'' :: Ord t => Distance
                      -> Double
                      -> Map (t, t) Int
                      -> Map (Index, Index) Double
cooc2graph'' distance threshold myCooc = neighbourMap
  where
    (ti, _) = createIndices myCooc
    myCooc' = toIndex ti myCooc
    matCooc = map2mat Triangle 0 (Map.size ti) $ Map.filter (> 1) myCooc'
    distanceMat  = measure distance matCooc
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





