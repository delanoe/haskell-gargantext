{-|
Module      : Gargantext.API.Count
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Count API part of Gargantext.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

module Gargantext.API.Search
      where

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import Servant
import Test.QuickCheck.Arbitrary
import Test.QuickCheck (elements)
-- import Control.Applicative ((<*>))
import Gargantext.Prelude
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Core.Types.Main (Offset, Limit)
import Gargantext.Database.Types.Node
import Gargantext.Database.TextSearch
import Gargantext.Database.Facet
import Gargantext.Database.Utils (Cmd)

-----------------------------------------------------------------------
-- | SearchIn [NodesId] if empty then global search
-- TODO [Int]
data SearchQuery = SearchQuery { sq_query :: [Text]
                               , sq_corpus_id :: NodeId
                               } deriving (Generic)
$(deriveJSON (unPrefix "sq_") ''SearchQuery)
instance ToSchema SearchQuery where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 3 fieldLabel}

instance Arbitrary SearchQuery where
  arbitrary = elements [SearchQuery ["electrodes"] 472764]

--

data SearchInQuery = SearchInQuery { siq_query :: [Text]
                               } deriving (Generic)
$(deriveJSON (unPrefix "siq_") ''SearchInQuery)
instance ToSchema SearchInQuery where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 4 fieldLabel}

instance Arbitrary SearchInQuery where
  arbitrary = SearchInQuery <$> arbitrary


-----------------------------------------------------------------------

data SearchResults = SearchResults { srs_results :: [FacetPaired Int UTCTime HyperdataDocument Int [Pair Int Text]]}
  deriving (Generic)
$(deriveJSON (unPrefix "srs_") ''SearchResults)

instance Arbitrary SearchResults where
  arbitrary = SearchResults <$> arbitrary

instance ToSchema SearchResults where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 4 fieldLabel}

-----------------------------------------------------------------------
type SearchAPI = Post '[JSON] SearchResults
-----------------------------------------------------------------------

search :: SearchQuery -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd err SearchResults
search (SearchQuery q pId) o l order =
  SearchResults <$> searchInCorpusWithContacts pId q o l order

searchIn :: NodeId -> SearchInQuery -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd err SearchResults
searchIn nId (SearchInQuery q ) o l order =
  SearchResults <$> searchInCorpusWithContacts nId q o l order


