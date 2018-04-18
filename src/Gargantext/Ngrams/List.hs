module Gargantext.Ngrams.List where

import Data.Maybe
import Data.List (filter)
import Gargantext.Ngrams
import Gargantext.Prelude

graph :: [Ngrams] -> [Ngrams]
graph ngs = filter (\ng -> _ngramsListName ng == Just Graph) ngs

candidates :: [Ngrams] -> [Ngrams]
candidates ngs = filter (\ng -> _ngramsListName ng == Just Candidate) ngs

stop :: [Ngrams] -> [Ngrams]
stop ngs = filter (\ng -> _ngramsListName ng == Just Stop) ngs

