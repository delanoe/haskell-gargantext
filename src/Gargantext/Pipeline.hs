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

import Data.Text.IO (readFile)

----------------------------------------------
----------------------------------------------
import Gargantext.Core
import Gargantext.Prelude

import Gargantext.Viz.Graph.Index (score)
import Gargantext.Viz.Graph.Distances.Matrice (distributional)
import Gargantext.Text.Metrics.Occurrences
import Gargantext.Text.Terms
import Gargantext.Text.Context


pipeline path = do
  -- Text  <- IO Text <- FilePath
  text     <- readFile path  
  let contexts = splitBy (Sentences 3) text
  myterms <- extractTerms Multi FR contexts
  -- TODO    filter (\t -> not . elem t stopList) myterms
  -- TODO    groupBy (Stem | GroupList)
  let myCooc = removeApax $ cooc myterms
  -- Cooc -> Matrix
  pure $ score distributional myCooc
  -- Matrix -> Clustering -> Graph -> JSON

