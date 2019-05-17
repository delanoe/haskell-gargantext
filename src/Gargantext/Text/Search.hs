{-|
Module      : Gargantext.Text.Search
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

This search Engine is first made to clean CSV file according to a query.

Starting from this model, a specific Gargantext engine will be made
(using more metrics scores/features).
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Gargantext.Text.Search where

import Data.SearchEngine

import Data.Ix

-- Usefull to use stopwords
-- import Data.Set (Set)
-- import qualified Data.Set as Set
import Data.Text (Text)

import Gargantext.Prelude
import Gargantext.Text.Terms.Mono (monoTexts)
import Gargantext.Text.Terms.Mono.Stem as ST
import Gargantext.Text.Parsers.CSV

type DocId = Int

type DocSearchEngine = SearchEngine
                         CsvGargV3
                         DocId
                         DocField
                         NoFeatures

data DocField = TitleField
              | AbstractField
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

initialDocSearchEngine :: DocSearchEngine
initialDocSearchEngine =
    initSearchEngine docSearchConfig defaultSearchRankParameters

docSearchConfig :: SearchConfig CsvGargV3 DocId DocField NoFeatures
docSearchConfig =
    SearchConfig {
      documentKey           = d_docId,
      extractDocumentTerms  = extractTerms,
      transformQueryTerm    = normaliseQueryToken,
      documentFeatureValue  = const noFeatures
  }
  where
    extractTerms :: CsvGargV3 -> DocField -> [Text]
    extractTerms doc TitleField       = monoTexts (d_title doc)
    extractTerms doc AbstractField    = monoTexts (d_abstract doc)

    normaliseQueryToken :: Text -> DocField -> Text
    normaliseQueryToken tok =
      let tokStem = ST.stem ST.EN
       in \field -> case field of
                      TitleField    -> tokStem tok
                      AbstractField -> tokStem tok

defaultSearchRankParameters :: SearchRankParameters DocField NoFeatures
defaultSearchRankParameters =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights     = noFeatures,
      paramFeatureFunctions   = noFeatures,
      paramResultsetSoftLimit = 2000,
      paramResultsetHardLimit = 4000,
      paramAutosuggestPrefilterLimit  = 500,
      paramAutosuggestPostfilterLimit = 500
    }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: DocField -> Float
    paramB TitleField      = 0.9
    paramB AbstractField   = 0.5

    paramFieldWeights :: DocField -> Float
    paramFieldWeights TitleField    = 20
    paramFieldWeights AbstractField = 5

