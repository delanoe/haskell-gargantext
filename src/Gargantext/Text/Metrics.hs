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

--import Data.Array.Accelerate ((:.)(..), Z(..))
--import Math.KMeans (kmeans, euclidSq, elements)

--import GHC.Float (exp)

import Data.Map (Map)
import Data.List.Extra (sortOn)
import GHC.Real (round)
import Gargantext.Prelude
import Gargantext.Viz.Graph.Distances.Matrice
import Gargantext.Viz.Graph.Index
import qualified Data.Array.Accelerate as DAA
import qualified Data.Array.Accelerate.Interpreter as DAA
import qualified Data.List as List
import qualified Data.Map  as Map

import Numeric.Statistics.PCA (pcaReduceN)
import qualified Data.Vector.Storable as Vec
import Data.Array.IArray (Array, listArray, elems)

type GraphListSize = Int
type InclusionSize = Int



toScored :: Ord t => [Map t (Vec.Vector Double)] -> [Scored t] 
toScored = map2scored
         . (reduceTo (Dimension 2))
         . (Map.filter (\v -> Vec.length v > 1))
         . (Map.unionsWith (<>))


scored :: Ord t => Map (t,t) Int -> [Scored t]
scored = map2scored . (reduceTo (Dimension 2)) . scored2map

scored2map :: Ord t => Map (t,t) Int -> Map t (Vec.Vector Double)
scored2map m = Map.fromList $ map (\(Scored t i s) -> (t, Vec.fromList [i,s])) $ scored' m

map2scored :: Ord t => Map t (Vec.Vector Double) -> [Scored t]
map2scored = map (\(t, ds) -> Scored t (Vec.head ds) (Vec.last ds)) . Map.toList

-- TODO change type with (x,y)
data Scored ts = Scored
  { _scored_terms  :: !ts
  , _scored_incExc :: !InclusionExclusion
  , _scored_speGen :: !SpecificityGenericity
  } deriving (Show)

data Dimension = Dimension Int

reduceTo :: Ord t
         => Dimension
         -> Map t (Vec.Vector Double)
         -> Map t (Vec.Vector Double)
reduceTo (Dimension d) ss = Map.fromList $ zip txts $ elems $ pcaReduceN ss'' d
  where
    ss'' :: Array Int (Vec.Vector Double)
    ss'' = listArray (1, List.length ss') ss'

    (txts,ss') = List.unzip $ Map.toList ss


localMetrics :: Ord t => Map (t,t) Int -> Map t (Vec.Vector Double)
localMetrics m = Map.fromList $ zipWith (\(_,t) (inc,spe) -> (t, Vec.fromList [inc,spe]))
                                       (Map.toList fi)
                                       scores
  where
    (ti, fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores   = DAA.toList
             $ DAA.run
             $ DAA.zip (DAA.use is) (DAA.use ss)




-- TODO Code to be remove below
-- TODO in the textflow we end up needing these indices , it might be
-- better to compute them earlier and pass them around.
scored' :: Ord t => Map (t,t) Int -> [Scored t]
scored' m = zipWith (\(_,t) (inc,spe) -> Scored t (inc) (spe)) (Map.toList fi) scores
  where
    (ti, fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores   = DAA.toList
             $ DAA.run
             $ DAA.zip (DAA.use is) (DAA.use ss)






takeScored :: Ord t => GraphListSize -> InclusionSize -> Map (t,t) Int -> [t]
takeScored listSize incSize = map _scored_terms
                            . linearTakes listSize incSize _scored_speGen
                                                           _scored_incExc
                            . scored


-- | Filter Scored data
-- >>> linearTakes 2 3 fst snd $ Prelude.zip ([1..10] :: [Int]) (reverse $ [1..10] :: [Int])
-- [(3,8),(6,5)]
linearTakes :: (Ord b1, Ord b2)
            => GraphListSize -> InclusionSize
            -> (a -> b2) -> (a -> b1) -> [a] -> [a]
linearTakes gls incSize speGen incExc = take gls
                      . List.concat
                      . map (take $ round
                                  $ (fromIntegral gls     :: Double)
                                  / (fromIntegral incSize :: Double)
                             )
                      . map (sortOn incExc)
                      . splitEvery incSize
                      . sortOn speGen


