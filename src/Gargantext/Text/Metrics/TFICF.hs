{-|
Module      : Gargantext.Text.Metrics.TFICF
Description : TFICF Ngrams tools
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Definition of TFICF : Term Frequency - Inverse of Context Frequency

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Metrics.TFICF where

--import Data.Text (Text)
import Gargantext.Prelude

data TficfContext n m = TficfInfra n m
                      | TficfSupra n m
  deriving (Show)

data Total = Total {unTotal :: !Double}
data Count = Count {unCount :: !Double}

-- | TFICF is a generalization of TFIDF
-- https://en.wikipedia.org/wiki/Tf%E2%80%93idf
tficf :: TficfContext Total Count -> TficfContext Total Count -> Double
tficf (TficfInfra (Total it) (Count ic))
      (TficfSupra (Total st) (Count sc))
      = tficf' it ic st sc
          where
            tficf' :: Double -> Double -> Double -> Double -> Double
            tficf' it' ic' st' sc'
                  | it' >= ic' && st' >= sc' = (ic'/it') / log (sc'/st')
                  | otherwise         = panic "Frequency impossible"
tficf _ _ = panic "Undefined for these contexts"


