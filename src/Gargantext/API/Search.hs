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

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Swagger hiding (fieldLabelModifier, Contact)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.Utils.Prefix (unPrefixSwagger, unCapitalize, dropPrefix)
import Gargantext.Database.Query.Facet
import Gargantext.Database.Action.Search
import Gargantext.Database.Action.Flow.Pairing (isPairedWith)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataContact(..), HyperdataDocument(..), ContactWho(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude
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
api :: NodeId -> GargServer (API SearchResult)
api nId (SearchQuery q SearchDoc) o l order =
  SearchResult <$> SearchResultDoc <$> map toRow <$> searchInCorpus nId False q o l order
api nId (SearchQuery q SearchContact) o l order = do
  printDebug "isPairedWith" nId
  aIds <- isPairedWith nId NodeAnnuaire
  -- TODO if paired with several corpus
  case head aIds of
    Nothing  -> pure $ SearchResult $ SearchNoResult "[G.A.Search] pair corpus with an Annuaire"
    Just aId -> SearchResult <$> SearchResultContact <$> map toRow <$> searchInCorpusWithContacts nId aId q o l order
api _ _ _ _ _ = undefined

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- | Main Types
-----------------------------------------------------------------------
data SearchType = SearchDoc | SearchContact
  deriving (Generic)

instance FromJSON  SearchType
  where
    parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON  SearchType
  where
    toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToSchema SearchType
instance Arbitrary SearchType where
  arbitrary = elements [SearchDoc, SearchContact]

-----------------------------------------------------------------------
data SearchQuery =
  SearchQuery { query    :: ![Text]
              , expected :: !SearchType
              }
  | SearchQueryErr !Text
    deriving (Generic)

instance FromJSON  SearchQuery
  where
    parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON  SearchQuery
  where
    toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

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
  SearchResult { result :: !SearchResultTypes
              }
  | SearchResultErr !Text
    deriving (Generic)

instance FromJSON  SearchResult
  where
    parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON  SearchResult
  where
    toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToSchema SearchResult
{-
  where
    declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")
-}

instance Arbitrary SearchResult where
  arbitrary = SearchResult <$> arbitrary


data SearchResultTypes = SearchResultDoc { docs     :: ![Row]}
                  | SearchResultContact  { contacts :: ![Row] }
                  | SearchNoResult      { message  :: !Text }

  deriving (Generic)

instance FromJSON  SearchResultTypes
  where
    parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON  SearchResultTypes
  where
    toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
 
instance Arbitrary SearchResultTypes where
  arbitrary = do
    srd <- SearchResultDoc     <$> arbitrary
    src <- SearchResultContact <$> arbitrary
    srn <- pure $ SearchNoResult "No result because.."
    elements [srd, src, srn]

instance ToSchema SearchResultTypes where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")


--------------------------------------------------------------------

data Row =
  Document { id         :: !NodeId
           , created    :: !UTCTime
           , title      :: !Text
           , hyperdata  :: !HyperdataRow
           , category   :: !Int
           , score      :: !Int
           }
  | Contact  { c_id         :: !Int
             , c_created    :: !UTCTime
             , c_hyperdata  :: !HyperdataRow
             , c_score      :: !Int
             }
  deriving (Generic)

instance FromJSON  Row
  where
    parseJSON = genericParseJSON 
                 ( defaultOptions { sumEncoding = ObjectWithSingleField 
                                  }
                 )

instance ToJSON  Row
  where
    toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
 
instance Arbitrary Row where
  arbitrary = arbitrary

instance ToSchema Row where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")

class ToRow a where
  toRow :: a -> Row

instance ToRow FacetDoc where
  toRow (FacetDoc nId utc t h mc md) = Document nId utc t (toHyperdataRow h) (fromMaybe 0 mc) (round $ fromMaybe 0 md)

-- | TODO rename FacetPaired
type FacetContact = FacetPaired Int UTCTime HyperdataContact Int

instance ToRow FacetContact where
  toRow (FacetPaired nId utc h s) = Contact nId utc (toHyperdataRow h) s


--------------------------------------------------------------------
data HyperdataRow =
  HyperdataRowDocument { _hr_bdd                :: !Text
                       , _hr_doi                :: !Text
                       , _hr_url                :: !Text
                       , _hr_uniqId             :: !Text
                       , _hr_uniqIdBdd          :: !Text
                       , _hr_page               :: !Int
                       , _hr_title              :: !Text
                       , _hr_authors            :: !Text
                       , _hr_institutes         :: !Text
                       , _hr_source             :: !Text
                       , _hr_abstract           :: !Text
                       , _hr_publication_date   :: !Text
                       , _hr_publication_year   :: !Int
                       , _hr_publication_month  :: !Int
                       , _hr_publication_day    :: !Int
                       , _hr_publication_hour   :: !Int
                       , _hr_publication_minute :: !Int
                       , _hr_publication_second :: !Int
                       , _hr_language_iso2      :: !Text
                       }
  | HyperdataRowContact { _hr_firstname :: !Text
                        , _hr_lastname  :: !Text
                        , _hr_labs      :: !Text
                        }
  deriving (Generic)

instance FromJSON  HyperdataRow
  where
    parseJSON = genericParseJSON
              ( defaultOptions
                { sumEncoding = ObjectWithSingleField
                , fieldLabelModifier = unCapitalize . dropPrefix "_hr_"
                , omitNothingFields = False
                }
              )

instance ToJSON  HyperdataRow
  where
    toJSON = genericToJSON
               ( defaultOptions
                { sumEncoding = ObjectWithSingleField
                , fieldLabelModifier = unCapitalize . dropPrefix "_hr_"
                , omitNothingFields = False
                }
              )

instance Arbitrary HyperdataRow where
  arbitrary = arbitrary

instance ToSchema HyperdataRow where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_hr_")

class ToHyperdataRow a where
  toHyperdataRow :: a -> HyperdataRow

instance ToHyperdataRow HyperdataDocument where
  toHyperdataRow (HyperdataDocument b d u ui ub p t a i s abs' pd py pm pda ph pmin psec l) =
    HyperdataRowDocument
      (fromMaybe "" b)
      (fromMaybe "" d)
      (fromMaybe "" u)
      (fromMaybe "" ui)
      (fromMaybe "" ub)
      (fromMaybe 0 p)
      (fromMaybe "Title" t)
      (fromMaybe "" a)
      (fromMaybe "" i)
      (fromMaybe "" s)
      (fromMaybe "" abs')
      (fromMaybe "" pd)
      (fromMaybe 2020 py)
      (fromMaybe 1 pm)
      (fromMaybe 1 pda)
      (fromMaybe 1 ph)
      (fromMaybe 1 pmin)
      (fromMaybe 1 psec)
      (fromMaybe "EN" l)

instance ToHyperdataRow HyperdataContact where
  toHyperdataRow (HyperdataContact _ (Just (ContactWho _ fn ln _ _)) _ _ _ _ _ _ ) = HyperdataRowContact (fromMaybe "FN" fn) (fromMaybe "LN" ln) "Labs"
  toHyperdataRow (HyperdataContact _ _ _ _ _ _ _ _ ) = HyperdataRowContact "FirstName" "LastName" "Labs"
