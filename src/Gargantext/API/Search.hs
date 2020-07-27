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

import Data.Aeson
import Data.Swagger
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Action.Search
import Gargantext.Database.Action.Flow.Pairing (isPairedWith)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataContact)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

-----------------------------------------------------------------------
data SearchType = SearchDoc | SearchContact
  deriving (Generic)


instance FromJSON  SearchType where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON  SearchType where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
 
instance ToSchema SearchType
instance Arbitrary SearchType where
  arbitrary = elements [SearchDoc, SearchContact]

-----------------------------------------------------------------------
data SearchQuery =
  SearchQuery { query    :: ![Text]
              , expected :: !SearchType
              } deriving (Generic)

instance FromJSON  SearchQuery where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON  SearchQuery where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
 
instance ToSchema SearchQuery where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")

instance Arbitrary SearchQuery where
  arbitrary = elements [SearchQuery ["electrodes"] SearchDoc]
-----------------------------------------------------------------------

data SearchResult = SearchResultDoc     { docs     :: ![FacetDoc]}
                  | SearchResultContact { contacts :: ![FacetPaired Int UTCTime HyperdataContact Int] }
                  | SearchNoResult      { message  :: !Text }

  deriving (Generic)

instance FromJSON  SearchResult where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON  SearchResult where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
 
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
api nId (SearchQuery q SearchContact) o l order = do
  aIds <- isPairedWith NodeAnnuaire nId
  -- TODO if paired with several corpus
  case head aIds of
    Nothing  -> pure $ SearchNoResult "[G.A.Search] pair corpus with an Annuaire"
    Just aId -> SearchResultContact <$> searchInCorpusWithContacts nId aId q o l order
-----------------------------------------------------------------------
