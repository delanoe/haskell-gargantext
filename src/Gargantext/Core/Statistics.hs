{-|
Module      : Gargantext.Core.Statistics
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Core.Statistics
  where


import Data.Map (Map)
import Gargantext.Prelude
import Numeric.Statistics.PCA (pcaReduceN)
import Data.Array.IArray (Array, listArray, elems)
import qualified Data.Vector.Storable as Vec
import qualified Data.List as List
import qualified Data.Map  as Map


data Dimension = Dimension Int

pcaReduceTo :: Ord t
         => Dimension
         -> Map t (Vec.Vector Double)
         -> Map t (Vec.Vector Double)
pcaReduceTo (Dimension d) m = Map.fromList
                             $ zip txts
                             $ elems
                             $ pcaReduceN m'' d
  where
    m'' :: Array Int (Vec.Vector Double)
    m'' = listArray (1, List.length m') m'

    (txts,m') = List.unzip $ Map.toList m


