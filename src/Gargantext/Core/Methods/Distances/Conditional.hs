{-|
Module      : Gargantext.Core.Methods.Distances
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Motivation and definition of the @Conditional@ distance.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE Strict            #-}
module Gargantext.Core.Methods.Distances.Conditional
  where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Hashable (Hashable)
import Data.List (unzip)
import Data.Maybe (catMaybes)
import Gargantext.Prelude
import qualified Data.HashMap.Strict as Map
import qualified Data.Set            as Set
import Control.DeepSeq (NFData)
type HashMap = Map.HashMap

------------------------------------------------------------------------
-- First version as first implementation
-- - qualitatively verified
-- - parallized as main optimization
conditional :: (Ord a, Hashable a, NFData a)
            => HashMap (a,a) Int
            -> HashMap (a,a) Double
conditional m' = Map.fromList $ ((catMaybes results') `using` parList rdeepseq)
  where
    results' = [ let
                  ij = (/) <$> Map.lookup (i,j) m <*> Map.lookup (i,i) m
                  ji = (/) <$> Map.lookup (j,i) m <*> Map.lookup (j,j) m
                  in getMax (i,j) ij ji

               | i <- keys
               , j <- keys
               , i < j
               ]
    -- Converting from Int to Double
    m       = Map.map fromIntegral m'

    -- Get the matrix coordinates, removing duplicates
    keys    = Set.toList $ Set.fromList (x <> y)
    (x,y)   = unzip $ Map.keys m


getMax :: (a,a)
       -> Maybe Double
       -> Maybe Double
       -> Maybe ((a,a), Double)
getMax (i,j) (Just d) Nothing   = Just ((i,j), d)
getMax (i,j) Nothing (Just d)   = Just ((j,i), d)
getMax ij   (Just di) (Just dj) = if di >= dj then getMax ij (Just di) Nothing
                                              else getMax ij Nothing   (Just dj)
getMax _ _ _ = Nothing


