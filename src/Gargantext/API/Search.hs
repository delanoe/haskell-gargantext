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
import Gargantext.API.Types (GargServer)
import Gargantext.Prelude
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Types.Node
import Gargantext.Database.TextSearch
import Gargantext.Database.Facet

-----------------------------------------------------------------------
data SearchQuery = SearchQuery
  { sq_query :: [Text]
  } deriving (Generic)

$(deriveJSON (unPrefix "sq_") ''SearchQuery)

instance ToSchema SearchQuery where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = drop 3}

instance Arbitrary SearchQuery where
  arbitrary = elements [SearchQuery ["electrodes"]]

-----------------------------------------------------------------------

data SearchDocResults = SearchDocResults { sdr_results :: [FacetDoc]}
  deriving (Generic)
$(deriveJSON (unPrefix "sdr_") ''SearchDocResults)

instance Arbitrary SearchDocResults where
  arbitrary = SearchDocResults <$> arbitrary

instance ToSchema SearchDocResults where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = drop 4}

data SearchPairedResults = SearchPairedResults { spr_results :: [FacetPaired Int UTCTime HyperdataDocument Int [Pair Int Text]] }
  deriving (Generic)
$(deriveJSON (unPrefix "spr_") ''SearchPairedResults)

instance Arbitrary SearchPairedResults where
  arbitrary = SearchPairedResults <$> arbitrary

instance ToSchema SearchPairedResults where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 4 fieldLabel}

-----------------------------------------------------------------------
-- TODO-ACCESS: CanSearch? or is it part of CanGetNode
-- TODO-EVENTS: No event, this is a read-only query.
type SearchAPI results
  = Summary "Search endpoint"
 :> ReqBody '[JSON] SearchQuery
 :> QueryParam "offset" Int
 :> QueryParam "limit"  Int
 :> QueryParam "order"  OrderBy
 :> Post '[JSON] results

type SearchDocsAPI  = SearchAPI SearchDocResults
type SearchPairsAPI = SearchAPI SearchPairedResults
-----------------------------------------------------------------------

searchPairs :: NodeId -> GargServer SearchPairsAPI
searchPairs pId (SearchQuery q) o l order =
  SearchPairedResults <$> searchInCorpusWithContacts pId q o l order

searchDocs :: NodeId -> GargServer SearchDocsAPI
searchDocs nId (SearchQuery q) o l order =
  SearchDocResults <$> searchInCorpus nId q o l order
  --SearchResults <$> searchInCorpusWithContacts nId q o l order


