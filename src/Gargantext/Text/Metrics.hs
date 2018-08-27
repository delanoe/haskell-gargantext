{-|
Module      : Gargantext.Text.Metrics
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Mainly reexport functions in @Data.Text.Metrics@

-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Metrics
  where

import Data.Ord (Down(..))
import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map  as M

--import Math.KMeans (kmeans, euclidSq, elements)

import Gargantext.Prelude
import Gargantext.Viz.Graph.Distances.Matrice
import Gargantext.Viz.Graph.Index

import qualified Data.Array.Accelerate.Interpreter as DAA
import qualified Data.Array.Accelerate as DAA
-- import Data.Array.Accelerate ((:.)(..), Z(..))

import GHC.Real (round)

import Debug.Trace (trace)

data MapListSize   = MapListSize   Int
data InclusionSize = InclusionSize Int
data SampleBins    = SampleBins    Double
data Clusters      = Clusters      Int
data DefaultValue  = DefaultValue  Int

data FilterConfig = FilterConfig { fc_mapListSize   :: MapListSize
                                 , fc_inclusionSize :: InclusionSize
                                 , fc_sampleBins    :: SampleBins
                                 , fc_clusters      :: Clusters
                                 , fc_defaultValue  :: DefaultValue
                             }

filterCooc :: (Show t, Ord t) => FilterConfig -> Map (t, t) Int -> Map (t, t) Int
filterCooc fc cc = (filterCooc' fc) ts cc
  where
    ts     = map _scored_terms $ takeSome fc $ coocScored cc

filterCooc' :: (Show t, Ord t) => FilterConfig -> [t] -> Map (t, t) Int -> Map (t, t) Int
filterCooc' (FilterConfig _ _ _ _ (DefaultValue dv)) ts m = trace ("coocScored " <> show ts) $
  foldl' (\m' k -> M.insert k (maybe dv identity $ M.lookup k m) m')
    M.empty selection
  where
    selection  = [(x,y) | x <- ts
                        , y <- ts
                      --  , x >= y
                        ]


-- | Map list creation
-- Kmeans split into (Clusters::Int) main clusters with Inclusion/Exclusion (relevance score)
-- Sample the main cluster ordered by specificity/genericity in (SampleBins::Double) parts
-- each parts is then ordered by Inclusion/Exclusion
-- take n scored terms in each parts where n * SampleBins = MapListSize.
takeSome :: Ord t => FilterConfig -> [Scored t] -> [Scored t]
takeSome (FilterConfig (MapListSize l) (InclusionSize l') (SampleBins s) (Clusters _) _) scores = L.take l
                    $ takeSample n m
                    $ L.take l' $ reverse $ sortWith (Down . _scored_incExc) scores
                    -- splitKmeans k scores
  where
    -- TODO: benchmark with accelerate-example kmeans version
    --splitKmeans x xs = L.concat $ map elements
    --                 $ V.take (k-1)
    --                 $ kmeans (\i -> VU.fromList ([(_scored_incExc i :: Double)]))
    --                          euclidSq x xs
    n = round ((fromIntegral l)/s)
    m = round $ (fromIntegral $ length scores) / (s)
    takeSample n' m' xs = -- trace ("splitKmeans " <> show (length xs)) $
                        L.concat $ map (L.take n')
                                 $ map (sortWith (Down . _scored_incExc))
                                 -- TODO use kmeans s instead of splitEvery
                                 -- in order to split in s heteregenous parts
                                 -- without homogeneous order hypothesis
                                 $ splitEvery m'
                                 $ sortWith (Down . _scored_speGen) xs


data Scored ts = Scored { _scored_terms :: !ts
                        , _scored_incExc :: !InclusionExclusion
                        , _scored_speGen :: !SpecificityGenericity
                        } deriving (Show)

-- TODO in the textflow we end up needing these indices, it might be better
-- to compute them earlier and pass them around.
coocScored :: Ord t => Map (t,t) Int -> [Scored t]
coocScored m = zipWith (\(_,t) (inc,spe) -> Scored t inc spe) (M.toList fi) scores
  where
    (ti,fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores = DAA.toList $ DAA.run $ DAA.zip (DAA.use is) (DAA.use ss)
