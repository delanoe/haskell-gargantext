{-| Module      : Graph.Types
Description :
Copyright   : (c) CNRS, Alexandre Delanoë
License     : AGPL + CECILL v3
Maintainer  : contact@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Viz.Graph.Types where

import qualified Data.Graph.Inductive.PatriciaTree as DGIP
-- import Data.IntMap (IntMap)
-- import qualified Eigen.Matrix as DenseMatrix
-- import Eigen.SparseMatrix (SparseMatrix)

--import qualified Data.Matrix.Sparse.Static    as Sparse
-- import qualified Data.Vector.Unboxed          as VU
-- import qualified Numeric.LinearAlgebra.Static as Dense
import Protolude hiding (sum, natVal)

-- | Main Types use in this libray

type Dict = IntMap

-- | Use the optimized version of Graph
type Graph a b = DGIP.Gr a b

-- | Type for Matrix computation optimizations (with Eigen)
-- type MatrixD          n = Dense.L n n
-- type MatrixS          n = Sparse.Matrix n n Double

