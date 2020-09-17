{-|
Module      : Gargantext.Core.Text.Metrics
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Mainly reexport functions in @Data.Text.Metrics@

-}

{-# LANGUAGE BangPatterns      #-}

module Gargantext.Core.Text.Metrics
  where

--import Data.Array.Accelerate ((:.)(..), Z(..))
--import Math.KMeans (kmeans, euclidSq, elements)

--import GHC.Float (exp)
import Data.Tuple.Extra (both)
import Data.Map (Map)
import Data.List.Extra (sortOn)
import GHC.Real (round)
import Gargantext.Prelude
import Gargantext.Core.Viz.Graph.Distances.Matrice
import Gargantext.Core.Viz.Graph.Index
import Gargantext.Core.Statistics (pcaReduceTo, Dimension(..))
import qualified Data.Array.Accelerate as DAA
import qualified Data.Array.Accelerate.Interpreter as DAA
import qualified Data.List as List
import qualified Data.Map  as Map

import qualified Data.Vector.Storable as Vec

type MapListSize = Int
type InclusionSize = Int

{-
toScored' :: Ord t => [Map t (Vec.Vector Double)] -> [Scored t] 
toScored' = map2scored
         . (pcaReduceTo (Dimension 2))
         . (Map.filter (\v -> Vec.length v > 1))
         . (Map.unionsWith (<>))
-}

scored :: Ord t => Map (t,t) Int -> [Scored t]
scored = map2scored . (pcaReduceTo (Dimension 2)) . scored2map
  where
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

localMetrics' :: Ord t => Map (t,t) Int -> Map t (Vec.Vector Double)
localMetrics' m = Map.fromList $ zipWith (\(_,t) (inc,spe) -> (t, Vec.fromList [inc,spe]))
                                         (Map.toList fi)
                                          scores
  where
    (ti, fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores   = DAA.toList
             $ DAA.run
             $ DAA.zip (DAA.use is) (DAA.use ss)

-- TODO Code to be removed below
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


takeScored :: Ord t => MapListSize -> InclusionSize -> Map (t,t) Int -> ([t],[t])
takeScored listSize incSize = both (map _scored_terms)
                            . takeLinear listSize incSize _scored_speGen
                                                          _scored_incExc
                            . scored


-- | Filter Scored data
-- >>> takeLinear 2 3 fst snd $ Prelude.zip ([1..10] :: [Int]) (reverse $ [1..10] :: [Int])
-- [(3,8),(6,5)]
takeLinear :: (Ord b1, Ord b2)
            => MapListSize -> InclusionSize
            -> (a -> b2) -> (a -> b1) -> [a] -> ([a],[a])
takeLinear mls incSize speGen incExc = (List.splitAt mls)
                      . List.concat
                      . map (take $ round
                                  $ (fromIntegral mls     :: Double)
                                  / (fromIntegral incSize :: Double)
                             )
                      . map (sortOn speGen)
                      . splitEvery incSize
                      . take 5000
                      . takePercent (0.70)
                      . sortOn incExc

takePercent :: Double -> [a] -> [a]
takePercent l xs = List.take l' xs
  where
    l' = round $ l * (fromIntegral $ List.length xs)

splitTake :: (Int, a -> Bool) -> (Int, a -> Bool) -> [a] -> ([a], [a])
splitTake (a, af) (b, bf) xs = (mpa <> mpb, ca <> cb)
  where
    (mpa, ca) = List.splitAt a $ List.filter af xs
    (mpb, cb) = List.splitAt b $ List.filter bf xs

