{-|
Module      : Gargantext.Core.Viz.Graph.Tools
Description : Tools to build Graph
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Gargantext.Core.Viz.Graph.Tools
  where

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Swagger hiding (items)
import GHC.Float (sin, cos)
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Core.Methods.Similarities (Similarity(..), measure)
import Gargantext.Core.Methods.Similarities.Conditional (conditional)
import Gargantext.Core.Statistics
import Gargantext.Core.Viz.Graph
import Gargantext.Core.Viz.Graph.Bridgeness (bridgeness3, Partitions, ToComId(..))
import Gargantext.Core.Viz.Graph.Index (createIndices, toIndex, map2mat, mat2map, Index, MatrixShape(..))
import Gargantext.Core.Viz.Graph.Tools.IGraph (mkGraphUfromEdges, spinglass)
import Gargantext.Core.Viz.Graph.Tools.Infomap (infomap)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Core.Viz.Graph.Utils (edgesFilter, nodesFilter)
import Gargantext.Prelude
import Graph.Types (ClusterNode)
import IGraph.Random -- (Gen(..))
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.List                as List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.HashSet             as HashSet
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


cooc2graph' :: Ord t => Similarity
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
               -> MultiPartite
               -> Similarity
               -> Threshold
               -> Strength
               -> HashMap (NgramsTerm, NgramsTerm) Int
               -> IO Graph
cooc2graphWith Spinglass = cooc2graphWith' (spinglass 1)
cooc2graphWith Confluence= cooc2graphWith' (\x -> pure $ BAC.defaultClustering x)
cooc2graphWith Infomap   = cooc2graphWith' (infomap "-v -N2")
--cooc2graphWith Infomap   = cooc2graphWith' (infomap "--silent --two-level -N2")
                        -- TODO: change these options, or make them configurable in UI?


cooc2graphWith' :: ToComId a
               => Partitions a
               -> MultiPartite
               -> Similarity
               -> Threshold
               -> Strength
               -> HashMap (NgramsTerm, NgramsTerm) Int
               -> IO Graph
cooc2graphWith' doPartitions multi similarity threshold strength myCooc = do
  let (distanceMap, diag, ti) = doSimilarityMap similarity threshold strength myCooc
  distanceMap `seq` diag `seq` ti `seq` return ()

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
  length partitions `seq` return ()

  let
    !confluence' = BAC.computeConfluences 3 (Map.keys bridgeness') True
    !bridgeness' = bridgeness3 confluence' distanceMap
  pure $ data2graph multi ti diag bridgeness' confluence' partitions

type Reverse = Bool

doSimilarityMap :: Similarity
              -> Threshold
              -> Strength
              -> HashMap (NgramsTerm, NgramsTerm) Int
              -> ( Map (Int,Int) Double
                 , Map (Index, Index) Int
                 , Map NgramsTerm Index
                 )
doSimilarityMap Distributional threshold strength myCooc = (distanceMap, toIndex ti diag, ti)
  where
    -- TODO remove below
    (diag, theMatrix) = Map.partitionWithKey (\(x,y) _ -> x == y)
                      $ Map.fromList
                      $ HashMap.toList myCooc

    (ti, _it) = createIndices theMatrix
    tiSize  = Map.size ti

    similarities = (\m -> m `seq` m)
                 $ (\m -> m `seq` measure Distributional m)
                 $ (\m -> m `seq` map2mat Square 0 tiSize m)
                 $ theMatrix `seq` toIndex ti theMatrix

    links = round (let n :: Double = fromIntegral tiSize in n * (log n)^(2::Int))

    distanceMap = Map.fromList
                $ List.take links
                $ (if strength == Weak then List.reverse else identity)
                $ List.sortOn snd
                $ Map.toList
                $ edgesFilter
                $ (\m -> m `seq` Map.filter (> threshold) m)
                $ similarities `seq` mat2map similarities

doSimilarityMap Conditional threshold strength myCooc = (distanceMap, toIndex ti myCooc', ti)
  where
    myCooc' = Map.fromList $ HashMap.toList myCooc
    (ti, _it) = createIndices myCooc'
    links = round (let n :: Double = fromIntegral (Map.size ti) in n * (log n)^(2::Int))
    distanceMap = toIndex ti
                $ Map.fromList
                $ List.take links
                $ (if strength == Weak then List.reverse else identity)
                $ List.sortOn snd
                $ HashMap.toList
                $ HashMap.filter (> threshold)
                $ conditional myCooc

----------------------------------------------------------
-- | From data to Graph
type Occurrences      = Int

nodeTypeWith :: MultiPartite -> NgramsTerm -> NgramsType
nodeTypeWith (MultiPartite (Partite s1 t1) (Partite _s2 t2)) t =
  if HashSet.member t s1
     then t1
     else t2


data2graph :: ToComId a
           => MultiPartite
           -> Map NgramsTerm Int
           -> Map (Int, Int) Occurrences
           -> Map (Int, Int) Double
           -> Map (Int, Int) Double
           -> [a]
           -> Graph
data2graph multi labels' occurences bridge conf partitions =
  Graph { _graph_nodes = nodes
        , _graph_edges = edges
        , _graph_metadata = Nothing
        }

   where

    nodes = map (setCoord ForceAtlas labels bridge)
          [ (n, Node { node_size    = maybe 0 identity (Map.lookup (n,n) occurences)
                     , node_type    = nodeTypeWith multi label
                     , node_id      = (cs . show) n
                     , node_label   = unNgramsTerm label
                     , node_x_coord = 0
                     , node_y_coord = 0
                     , node_attributes =
                              Attributes { clust_default = fromMaybe 0
                                                           (Map.lookup n community_id_by_node_id)
                                         }
                     , node_children = []
                     }
               )
            | (label, n) <- labels
            , Set.member n toKeep
            ]

    (bridge', toKeep) = nodesFilter (\v -> v > 1) bridge

    edges = [ Edge { edge_source = cs (show s)
                   , edge_target = cs (show t)
                   , edge_weight = weight
                   , edge_confluence = maybe 0 identity $ Map.lookup (s,t) conf
                   , edge_id     = cs (show i)
                   }
            | (i, ((s,t), weight)) <- zip ([0..]::[Integer] ) $ Map.toList bridge'
            , s /= t
            , weight > 0
            ]

    community_id_by_node_id = Map.fromList
                            $ map nodeId2comId partitions

    labels = Map.toList labels'


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
cooc2graph'' :: Ord t => Similarity
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

