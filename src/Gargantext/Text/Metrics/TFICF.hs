{-|
Module      : Gargantext.Text.Metrics.TFICF
Description : TFICF Ngrams tools
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Definition of TFICF : Term Frequency - Inverse of Context Frequency

TFICF is a generalization of [TFIDF](https://en.wikipedia.org/wiki/Tf%E2%80%93idf).

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Metrics.TFICF where

import Gargantext.Prelude

type TFICF = Double

data TficfContext n m = TficfInfra n m
                      | TficfSupra n m
  deriving (Show)

data Total = Total {unTotal :: !Double}
data Count = Count {unCount :: !Double}

tficf :: TficfContext Count Total
      -> TficfContext Count Total
      -> TFICF
tficf (TficfInfra (Count ic) (Total it) )
      (TficfSupra (Count sc) (Total st) )
            | it >= ic && st >= sc = (ic/it) / log (sc/st)
            | otherwise            = panic "Frequency impossible"
tficf _ _ = panic "Undefined for these contexts"


