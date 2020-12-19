{-|
Module      : Gargantext.Database.Schema.NgramsPostag
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Each Ngrams has a pos-tagging version to ease the default groups of
ngrams in NgramsTerm Lists.

-}

{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NgramsPostag
  where

import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Database.Schema.Prelude

data NgramsPosTagPoly id
                      lang_id
                      algo_id
                      postag
                      ngrams_id
                      lemm_id
                      score
  = NgramsPosTagDB { _ngramsPosTag_id        :: !id
                   , _ngramsPosTag_lang_id   :: !lang_id
                   , _ngramsPosTag_algo_id   :: !algo_id
                   , _ngramsPosTag_postag    :: !postag
                   , _ngramsPosTag_ngrams_id :: !ngrams_id
                   , _ngramsPosTag_lemm_id   :: !lemm_id
                   , _ngramsPosTag_score     :: !score
                   } deriving (Show)

------------------------------------------------------------------------

type NgramsPosTagWrite = NgramsPosTagPoly (Maybe (Column PGInt4))
                                   (Column PGInt4)
                                   (Column PGInt4)
                                   (Maybe (Column PGText))
                                   (Column PGInt4)
                                   (Column PGInt4)
                                   (Maybe (Column PGInt4))

type NgramsPosTagRead  = NgramsPosTagPoly (Column PGInt4)
                                   (Column PGInt4)
                                   (Column PGInt4)
                                   (Column PGText)
                                   (Column PGInt4)
                                   (Column PGInt4)
                                   (Column PGInt4)

type NgramsPosTagReadNull =  NgramsPosTagPoly (Column (Nullable PGInt4))
                                   (Column (Nullable PGInt4))
                                   (Column (Nullable PGInt4))
                                   (Column (Nullable PGText))
                                   (Column (Nullable PGInt4))
                                   (Column (Nullable PGInt4))
                                   (Column (Nullable PGInt4))

type NgramsPosTagDB = NgramsPosTagPoly Int Int Int Text Int Int Int
