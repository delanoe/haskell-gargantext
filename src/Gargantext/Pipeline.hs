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

{-# LANGUAGE NoImplicitPrelude    #-}

module Gargantext.Pipeline
  where


import Data.Text.IO (readFile)

import Gargantext.Core
import Gargantext.Prelude

import Gargantext.Text.Metrics.Occurrences
import Gargantext.Text.Terms
import Gargantext.Text.Context


pipeline pth = do
  text     <- readFile pth
  let contexts = splitBy Sentences 4 text
  cooc <$> map occurrences <$> mapM (terms Mono FR) contexts
  -- todo
  -- Cooc map -> Matrix
  -- distributional or conditional
  -- Matrix -> Graph

