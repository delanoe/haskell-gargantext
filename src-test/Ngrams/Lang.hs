
{-|
Module      : Ngrams.Lang
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Ngrams.Lang where

import Gargantext.Prelude (IO())

import Gargantext.Core (Lang(..))
import qualified Ngrams.Lang.Fr as Fr
import qualified Ngrams.Lang.En as En

ngramsExtractionTest :: Lang -> IO ()
ngramsExtractionTest FR = Fr.ngramsExtractionTest
ngramsExtractionTest EN = En.ngramsExtractionTest

