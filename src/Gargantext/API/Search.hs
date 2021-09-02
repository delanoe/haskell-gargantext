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
import Data.Maybe (fromMaybe)
import Data.Swagger hiding (fieldLabelModifier, Contact)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.Utils.Prefix (unPrefixSwagger, unCapitalize, dropPrefix)
import Gargantext.Database.Action.Flow.Pairing (isPairedWith)
import Gargantext.Database.Action.Search
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataContact(..), HyperdataDocument(..), ContactWho(..))
import Gargantext.Database.Admin.Types.Hyperdata.Contact (_cw_organization)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Facet
import Gargantext.Prelude
import Gargantext.Utils.Aeson (defaultTaggedObject)
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import qualified Data.Text as Text

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
  SearchResult { result :: !SearchResultTypes}
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


data SearchResultTypes =
    SearchResultDoc { docs     :: ![Row] }
  | SearchResultContact  { contacts :: ![Row] }
  | SearchNoResult      { message  :: !Text }
  deriving (Generic)
instance FromJSON  SearchResultTypes
  where
    parseJSON = genericParseJSON (defaultOptions { sumEncoding = defaultTaggedObject })
instance ToJSON  SearchResultTypes
  where
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
             , c_annuaireId :: !NodeId
             }
  deriving (Generic)
instance FromJSON  Row
  where
    parseJSON = genericParseJSON 
                 ( defaultOptions { sumEncoding = defaultTaggedObject } )
instance ToJSON  Row
  where
    toJSON = genericToJSON (defaultOptions { sumEncoding = defaultTaggedObject })
instance Arbitrary Row where
  arbitrary = arbitrary

instance ToSchema Row where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "")

class ToRow a where
  toRow :: NodeId -> a -> Row

instance ToRow FacetDoc where
  toRow _ (FacetDoc nId utc t h mc _md sc) =
    Document nId utc t (toHyperdataRow h) (fromMaybe 0 mc) (round $ fromMaybe 0 sc)

-- | TODO rename FacetPaired
type FacetContact = FacetPaired Int UTCTime HyperdataContact Int

instance ToRow FacetContact where
  toRow annuaireId (FacetPaired nId utc h s) = Contact nId utc (toHyperdataRow h) s annuaireId


--------------------------------------------------------------------
data HyperdataRow =
  HyperdataRowDocument { _hr_abstract           :: !Text
                       , _hr_authors            :: !Text
                       , _hr_bdd                :: !Text
                       , _hr_doi                :: !Text
                       , _hr_institutes         :: !Text
                       , _hr_language_iso2      :: !Text
                       , _hr_page               :: !Int
                       , _hr_publication_date   :: !Text
                       , _hr_publication_day    :: !Int
                       , _hr_publication_hour   :: !Int
                       , _hr_publication_minute :: !Int
                       , _hr_publication_month  :: !Int
                       , _hr_publication_second :: !Int
                       , _hr_publication_year   :: !Int
                       , _hr_source             :: !Text
                       , _hr_title              :: !Text
                       , _hr_url                :: !Text
                       , _hr_uniqId             :: !Text
                       , _hr_uniqIdBdd          :: !Text
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
                { sumEncoding = defaultTaggedObject
                , fieldLabelModifier = unCapitalize . dropPrefix "_hr_"
                , omitNothingFields = False
                }
              )
instance ToJSON  HyperdataRow
  where
    toJSON = genericToJSON
               ( defaultOptions
                { sumEncoding = defaultTaggedObject
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
  toHyperdataRow (HyperdataDocument { _hd_bdd = bdd
                                    , _hd_doi = doi
                                    , _hd_url = url'
                                    , _hd_uniqId = uniqId
                                    , _hd_uniqIdBdd = uniqIdBdd
                                    , _hd_page = page
                                    , _hd_title = t
                                    , _hd_authors = authors
                                    , _hd_institutes = institutes
                                    , _hd_source = source
                                    , _hd_abstract = abstract
                                    , _hd_publication_date = pdate
                                    , _hd_publication_year = pyear
                                    , _hd_publication_month = pmonth
                                    , _hd_publication_day = pday
                                    , _hd_publication_hour = phour
                                    , _hd_publication_minute = pminute
                                    , _hd_publication_second = psecond
                                    , _hd_language_iso2 = language }) =
    HyperdataRowDocument
      { _hr_abstract = fromMaybe "" abstract
      , _hr_authors = fromMaybe "" authors
      , _hr_bdd = fromMaybe "" bdd
      , _hr_doi = fromMaybe "" doi
      , _hr_institutes = fromMaybe "" institutes
      , _hr_language_iso2 = fromMaybe "EN" language
      , _hr_page = fromMaybe 0 page
      , _hr_publication_date = fromMaybe "" pdate
      , _hr_publication_day = fromMaybe 1 pday
      , _hr_publication_hour = fromMaybe 1 phour
      , _hr_publication_minute = fromMaybe 1 pminute
      , _hr_publication_month = fromMaybe 1 pmonth
      , _hr_publication_second = fromMaybe 1 psecond
      , _hr_publication_year = fromMaybe 2020 pyear
      , _hr_source = fromMaybe "" source
      , _hr_title = fromMaybe "Title" t
      , _hr_url = fromMaybe "" url'
      , _hr_uniqId = fromMaybe "" uniqId
      , _hr_uniqIdBdd = fromMaybe "" uniqIdBdd }

instance ToHyperdataRow HyperdataContact where
  toHyperdataRow (HyperdataContact _ (Just (ContactWho _ fn ln _ _)) ou _ _ _ _ _ ) =
    HyperdataRowContact (fromMaybe "FirstName" fn) (fromMaybe "LastName" ln) ou'
      where
        ou' = maybe "CNRS" (Text.intercalate " " . _cw_organization) (head ou)
  toHyperdataRow (HyperdataContact _ _ _ _ _ _ _ _ ) =
    HyperdataRowContact "FirstName" "LastName" "Labs"
