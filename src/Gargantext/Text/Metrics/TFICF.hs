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
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Metrics.TFICF ( TFICF
                                     , TficfContext(..)
                                     , Total(..)
                                     , Count(..)
                                     , tficf
                                     )
  where

import Data.Text (Text)
import Gargantext.Prelude

path :: Text
path = "Gargantext.Text.Metrics.TFICF"

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
            | otherwise            = panic $ "[ERR]" <> path <>" Frequency impossible"
tficf _ _ = panic $ "[ERR]" <> path <> "Undefined for these contexts"


