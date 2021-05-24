
{-|
Module      : Graph.Distance
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


module Graph.Distance where

import Test.Hspec

import Gargantext.Core.Methods.Matrix.Accelerate.Utils (cross', matrix)
import Gargantext.Prelude

test :: IO ()
test = hspec $ do
  describe "Cross" $ do
    let result = cross' $ matrix 3 ([1,1..] :: [Double])
    it "compare" $ do
      shouldBe result (matrix 3 ([2,2..] :: [Double]))

