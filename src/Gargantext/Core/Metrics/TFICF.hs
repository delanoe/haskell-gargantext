{-|
Module      : Gargantext.Core.Metrics.TFICF
Description : Core Metrics TFICF filtering and grouping
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Core.Metrics.TFICF
  where

import Data.Map 
import Gargantext.Prelude
import Gargantext.Database.Metrics.TFICF
import Gargantext.Database.Schema.Ngrams
import Gargantext.Text.Metrics.TFICF
import Gargantext.API.Ngrams


group :: TficfData -> Map NgramsType [NgramsElement] -> TficfData
group = undefined

filter :: TficfData -> [NgramsElement]
filter = undefined


