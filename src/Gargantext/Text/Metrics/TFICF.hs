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


module Gargantext.Text.Metrics.TFICF ( TFICF
                                     , TficfContext(..)
                                     , Total(..)
                                     , Count(..)
                                     , tficf
                                     , sortTficf
                                     )
  where

import qualified Data.List as List
import Data.Map.Strict (Map, toList)
import qualified Data.Ord as DO (Down(..))
import Data.Set (Set)
import Data.Text

import Gargantext.Prelude
import Gargantext.Core.Types (Ordering(..))

path :: Text
path = "[G.T.Metrics.TFICF]"

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
            | it >= ic && st >= sc {-&& it <= st-} = (ic/it) / log (sc/st)
            | otherwise            = panic $ "[ERR]" <> path <>" Frequency impossible"
tficf _ _ = panic $ "[ERR]" <> path <> "Undefined for these contexts"


sortTficf :: Ordering
          -> (Map Text (Double, Set Text))
          -> [   (Text,(Double, Set Text))]
sortTficf Down = List.sortOn (DO.Down . fst . snd) . toList
sortTficf Up   = List.sortOn (fst . snd) . toList

