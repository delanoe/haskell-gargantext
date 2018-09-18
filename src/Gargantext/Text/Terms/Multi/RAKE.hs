{-|
Module      : Gargantext.Text.Terms.Multi.RAKE
Description : Rapid automatic keyword extraction (RAKE)
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Personal notes for the integration of RAKE in Gargantext.

RAKE algorithm is a simple, rapid and effective algorithm to extract
keywords that is very sensitive to the quality of the stop word list.

Indeed, the very first step starts from the stop words list to cut the
text towards keywords extraction. The conTexT is the sentence level to
compute the coccurrences and occurrences which are divided to compute
the metric of one word. Multi-words metrics is equal to the sum of the
metrics of each word.

Finally The metrics highlight longer keywords which highly depends of
quality of the cut which depends on the quality of the stop word list.

As a consequence, to improve the effectiveness of RAKE algorithm, I am
wondering if some bayesian features could be added to increase stop word
list quality in time.

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Terms.Multi.RAKE (multiterms_rake, select, hardStopList)
  where

import GHC.Real (round)
import Data.Text (Text, pack)
import NLP.RAKE.Text

import Gargantext.Text.Terms.Stop (stopList)
import Gargantext.Prelude

select :: Double -> [a] -> [a]
select part ns = take n ns
  where
    n = round $ part * (fromIntegral $ length ns)


multiterms_rake :: Text -> [WordScore]
multiterms_rake = candidates hardStopList
                        defaultNosplit
                        defaultNolist   . pSplitter

-- | StopList
hardStopList :: StopwordsMap
hardStopList =  mkStopwordsStr stopList
