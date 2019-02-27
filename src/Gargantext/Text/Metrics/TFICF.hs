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
import Gargantext.Database.Schema.Ngrams (NgramsId, NgramsTerms)

data TficfContext n m = TficfLanguage n m
                      | TficfCorpus   n m
                      | TficfDocument n m
                      | TficfInfra n m
                      | TficfSupra n m
  deriving (Show)

data Tficf = Tficf
  { tficf_ngramsId :: NgramsId
  , tficf_ngramsTerms :: NgramsTerms
  , tficf_score       :: Double
  } deriving (Show)

data Tficf' = Tficf'
  { tficf'_terms :: NgramsTerms
  , tficf'_score :: Double
  } deriving (Show)



type SupraContext = TficfContext
type InfraContext = TficfContext

-- | TFICF is a generalization of TFIDF
-- https://en.wikipedia.org/wiki/Tf%E2%80%93idf
tficf :: InfraContext Double Double -> SupraContext Double Double -> Double
tficf (TficfCorpus c c')  (TficfLanguage l l') = tficf' c c' l l'
tficf (TficfDocument d d')(TficfCorpus   c c') = tficf' d d' c c'
tficf _ _ = panic "Not in definition"

tficf' :: Double -> Double -> Double -> Double -> Double
tficf' c c' l l'
    | c <= c' && l < l' = (l/l') / log (c/c')
    | otherwise        = panic "Frequency impossible"


tficf_example :: [(Double,Double,Double,Double)]
tficf_example = undefined
