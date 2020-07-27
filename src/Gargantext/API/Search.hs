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

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Gargantext.API.Search
      where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Action.Search
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataContact)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

-----------------------------------------------------------------------
data SearchType = SearchDoc | SearchContact
  deriving (Generic)

$(deriveJSON (unPrefix "") ''SearchType)
instance ToSchema SearchType
instance Arbitrary SearchType where
  arbitrary = elements [SearchDoc, SearchContact]

-----------------------------------------------------------------------
data SearchQuery = SearchQuery
  { sq_query :: [Text]
  , sq_type  :: SearchType
  } deriving (Generic)

$(deriveJSON (unPrefix "sq_") ''SearchQuery)

instance ToSchema SearchQuery where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "sq_")

instance Arbitrary SearchQuery where
  arbitrary = elements [SearchQuery ["electrodes"] SearchDoc]
-----------------------------------------------------------------------

data SearchResult = SearchResultDoc     { sr_result :: [FacetDoc]}
                  | SearchResultContact { sr_results :: [FacetPaired Int UTCTime HyperdataContact Int] }             | SearchNoResult      { sr_message :: Text }

  deriving (Generic)
$(deriveJSON (unPrefix "sr_") ''SearchResult)

instance Arbitrary SearchResult where
  arbitrary = do
    srd <- SearchResultDoc     <$> arbitrary
    src <- SearchResultContact <$> arbitrary
    srn <- pure $ SearchNoResult "No result because.."
    elements [srd, src, srn]

instance ToSchema SearchResult where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "sr_")

-----------------------------------------------------------------------
-- TODO-ACCESS: CanSearch? or is it part of CanGetNode
-- TODO-EVENTS: No event, this is a read-only query.
type API results = Summary "Search endpoint"
                 :> ReqBody '[JSON] SearchQuery
                 :> QueryParam "offset" Int
                 :> QueryParam "limit"  Int
                 :> QueryParam "order"  OrderBy
                 :> Post '[JSON] results
-----------------------------------------------------------------------
api :: NodeId -> GargServer (API SearchResult)
api nId (SearchQuery q SearchDoc) o l order =
  SearchResultDoc <$> searchInCorpus nId False q o l order
api nId (SearchQuery q SearchContact) o l order =
  -- SearchPairedResults <$> searchInCorpusWithContacts pId aId q o l order
  pure $ SearchNoResult "Need Implementation"
-----------------------------------------------------------------------
