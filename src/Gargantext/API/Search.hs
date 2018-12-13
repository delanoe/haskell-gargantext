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

module Gargantext.API.Search
      where

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
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

-----------------------------------------------------------------------
data SearchQuery = SearchQuery { sq_query :: [Text]
                               , sq_corpus_id :: Int
                               } deriving (Generic)
$(deriveJSON (unPrefix "sq_") ''SearchQuery)
instance ToSchema SearchQuery where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 3 fieldLabel}

instance Arbitrary SearchQuery where
  arbitrary = elements [SearchQuery ["electrodes"] 472764]

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

search :: Connection -> SearchQuery -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Handler SearchResults
search c (SearchQuery q pId) o l order =
  liftIO $ SearchResults <$> searchInCorpusWithContacts c pId q o l order
