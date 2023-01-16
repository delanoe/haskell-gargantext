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

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Gargantext.API.Search
      where

import Data.Aeson hiding (defaultTaggedObject)
-- import Data.List (concat)
import Data.Swagger hiding (fieldLabelModifier, Contact)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.Types.Search
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.Action.Flow.Pairing (isPairedWith)
import Gargantext.Database.Action.Search
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Facet
import Gargantext.Prelude
import Gargantext.Utils.Aeson (defaultTaggedObject)
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

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
-- | Api search function
api :: NodeId -> GargServer (API SearchResult)
api nId (SearchQuery q SearchDoc) o l order =
  SearchResult <$> SearchResultDoc
               <$> map (toRow nId)
               <$> searchInCorpus nId False q o l order
               -- <$> searchInCorpus nId False (concat q) o l order
api nId (SearchQuery q SearchContact) o l order = do
  printDebug "isPairedWith" nId
  aIds <- isPairedWith nId NodeAnnuaire
  -- TODO if paired with several corpus
  case head aIds of
    Nothing  -> pure $ SearchResult
              $ SearchNoResult "[G.A.Search] pair corpus with an Annuaire"
    Just aId -> SearchResult
            <$> SearchResultContact
            <$> map (toRow aId)
            <$> searchInCorpusWithContacts nId aId q o l order
api _nId (SearchQuery _q SearchDocWithNgrams) _o _l _order = undefined

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- | Main Types
-----------------------------------------------------------------------
data SearchType = SearchDoc | SearchContact | SearchDocWithNgrams
  deriving (Generic)
instance FromJSON SearchType where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
instance ToJSON SearchType where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
instance ToSchema SearchType
instance Arbitrary SearchType where
  arbitrary = elements [SearchDoc, SearchContact]

-----------------------------------------------------------------------
data SearchQuery =
  SearchQuery { query    :: ![Text]
              , expected :: !SearchType
              }
    deriving (Generic)
instance FromJSON SearchQuery where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON SearchQuery where
  toJSON = genericToJSON defaultOptions
instance ToSchema SearchQuery
{-
  where
    declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")
-}

instance Arbitrary SearchQuery where
  arbitrary = elements [SearchQuery ["electrodes"] SearchDoc]
  -- arbitrary = elements [SearchQuery "electrodes" 1 ] --SearchDoc]
-----------------------------------------------------------------------
data SearchResult =
  SearchResult { result :: !SearchResultTypes}
    deriving (Generic)

instance FromJSON SearchResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SearchResult where
  toJSON = genericToJSON defaultOptions

instance ToSchema SearchResult
{-
  where
    declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")
-}

instance Arbitrary SearchResult where
  arbitrary = SearchResult <$> arbitrary


data SearchResultTypes =
    SearchResultDoc { docs     :: ![Row] }
  | SearchResultContact  { contacts :: ![Row] }
  | SearchNoResult      { message  :: !Text }
  deriving (Generic)
instance FromJSON SearchResultTypes where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = defaultTaggedObject })
instance ToJSON SearchResultTypes where
  toJSON = genericToJSON (defaultOptions { sumEncoding = defaultTaggedObject })

instance Arbitrary SearchResultTypes where
  arbitrary = do
    srd <- SearchResultDoc     <$> arbitrary
    src <- SearchResultContact <$> arbitrary
    srn <- pure $ SearchNoResult "No result because.."
    elements [srd, src, srn]

instance ToSchema SearchResultTypes where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


--------------------------------------------------------------------
