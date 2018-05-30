{-|
Module      : Gargantext.Pipeline
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NoImplicitPrelude           #-}

module Gargantext.Pipeline
  where

import Data.Text (unpack)
import qualified Data.Text as DT

import Data.Text.IO (readFile)

----------------------------------------------
----------------------------------------------

import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Prelude

import Gargantext.Viz.Graph.Index (map', createIndexes)
import Gargantext.Viz.Graph.Distances.Matrice (distributional, int2double)
import Gargantext.Text.Metrics.Occurrences
import Gargantext.Text.Terms
import Gargantext.Text.Context

import Data.Array.Accelerate as A

pipeline pth = do
  text     <- readFile pth
  let contexts = splitBy Sentences 4 text
  myterms <- mapM (terms Multi FR) contexts
  -- todo filter stop words
  let myCooc = removeApax $ cooc myterms
  --pure myCooc
  -- Cooc map -> Matrix
  --pure $ createIndexes myCooc
  pure $ map' int2double myCooc
  -- Matrix -> Graph

