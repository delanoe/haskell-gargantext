{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Gargantext.Database.Admin.Types.Hyperdata where

import Control.Lens hiding (elements, (&))
import Data.Aeson
import Data.Aeson (Object, toJSON)
import Data.Aeson.Types (emptyObject)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Monoid (mempty)
import Data.Swagger
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import GHC.Generics (Generic)
import Opaleye (QueryRunnerColumnDefault, queryRunnerColumnDefault, PGJsonb, fieldQueryRunnerColumn)
import Protolude hiding (ByteString)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

import Gargantext.API.Ngrams.NTree (MyTree)
import Gargantext.Database.Admin.Types.Metrics (ChartMetrics(..), Metrics)
import Gargantext.Database.Prelude (fromField')
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Viz.Phylo (Phylo(..))
import Gargantext.Viz.Types (Histo(..))


data CodeType = JSON | Markdown | Haskell
  deriving (Generic)
instance ToJSON CodeType
instance FromJSON CodeType
instance ToSchema CodeType

------------------------------------------------------------------------
data StatusV3  = StatusV3 { statusV3_error  :: !(Maybe Text)
                          , statusV3_action :: !(Maybe Text)
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "statusV3_") ''StatusV3)

------------------------------------------------------------------------
data CorpusField = MarkdownField { _cf_text :: !Text }
                  | JsonField { _cf_title :: !Text
                              , _cf_desc  :: !Text
                              , _cf_query :: !Text
                              , _cf_authors :: !Text
                              -- , _cf_resources :: ![Resource]
                              }
                  | HaskellField { _cf_haskell :: !Text }
                  deriving (Generic)

$(deriveJSON (unPrefix "_cf_") ''CorpusField)
$(makeLenses ''CorpusField)

defaultCorpusField :: CorpusField
defaultCorpusField = MarkdownField "# title"

instance ToSchema CorpusField where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_cf_") proxy
    & mapped.schema.description ?~ "CorpusField"
    & mapped.schema.example ?~ toJSON defaultCorpusField

------------------------------------------------------------------------
data Chart =
    CDocsHistogram
  | CAuthorsPie
  | CInstitutesTree
  | CTermsMetrics
  deriving (Generic, Show, Eq)
instance ToJSON Chart
instance FromJSON Chart
instance ToSchema Chart

------------------------------------------------------------------------

-- Only Hyperdata types should be member of this type class.

------------------------------------------------------------------------
data HyperdataDocumentV3 = HyperdataDocumentV3 { hyperdataDocumentV3_publication_day    :: !(Maybe Int)
                                               , hyperdataDocumentV3_language_iso2      :: !(Maybe Text)
                                               , hyperdataDocumentV3_publication_second :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_minute :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_month  :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_hour   :: !(Maybe Int)
                                               , hyperdataDocumentV3_error              :: !(Maybe Text)
                                               , hyperdataDocumentV3_language_iso3      :: !(Maybe Text)
                                               , hyperdataDocumentV3_authors            :: !(Maybe Text)
                                               , hyperdataDocumentV3_publication_year   :: !(Maybe Int)
                                               , hyperdataDocumentV3_publication_date   :: !(Maybe Text)
                                               , hyperdataDocumentV3_language_name      :: !(Maybe Text)
                                               , hyperdataDocumentV3_statuses           :: !(Maybe [StatusV3])
                                               , hyperdataDocumentV3_realdate_full_     :: !(Maybe Text)
                                               , hyperdataDocumentV3_source             :: !(Maybe Text)
                                               , hyperdataDocumentV3_abstract           :: !(Maybe Text)
                                               , hyperdataDocumentV3_title              :: !(Maybe Text)
                                               } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataDocumentV3_") ''HyperdataDocumentV3)

class Hyperdata a
instance Hyperdata HyperdataDocumentV3

------------------------------------------------------------------------
data HyperdataDocument = HyperdataDocument { _hyperdataDocument_bdd                :: !(Maybe Text)
                                           , _hyperdataDocument_doi                :: !(Maybe Text)
                                           , _hyperdataDocument_url                :: !(Maybe Text)
                                           , _hyperdataDocument_uniqId             :: !(Maybe Text)
                                           , _hyperdataDocument_uniqIdBdd          :: !(Maybe Text)
                                           , _hyperdataDocument_page               :: !(Maybe Int)
                                           , _hyperdataDocument_title              :: !(Maybe Text)
                                           , _hyperdataDocument_authors            :: !(Maybe Text)
                                           , _hyperdataDocument_institutes         :: !(Maybe Text)
                                           , _hyperdataDocument_source             :: !(Maybe Text)
                                           , _hyperdataDocument_abstract           :: !(Maybe Text)
                                           , _hyperdataDocument_publication_date   :: !(Maybe Text)
                                           , _hyperdataDocument_publication_year   :: !(Maybe Int)
                                           , _hyperdataDocument_publication_month  :: !(Maybe Int)
                                           , _hyperdataDocument_publication_day    :: !(Maybe Int)
                                           , _hyperdataDocument_publication_hour   :: !(Maybe Int)
                                           , _hyperdataDocument_publication_minute :: !(Maybe Int)
                                           , _hyperdataDocument_publication_second :: !(Maybe Int)
                                           , _hyperdataDocument_language_iso2      :: !(Maybe Text)
                                           } deriving (Show, Generic)

$(deriveJSON (unPrefix "_hyperdataDocument_") ''HyperdataDocument)
$(makeLenses ''HyperdataDocument)

class ToHyperdataDocument a where
  toHyperdataDocument :: a -> HyperdataDocument

instance ToHyperdataDocument HyperdataDocument
  where
    toHyperdataDocument = identity

instance Eq HyperdataDocument where
  (==) h1 h2 = (==) (_hyperdataDocument_uniqId h1) (_hyperdataDocument_uniqId h2)

instance Ord HyperdataDocument where
  compare h1 h2 = compare (_hyperdataDocument_publication_date h1) (_hyperdataDocument_publication_date h2)

instance Hyperdata HyperdataDocument

instance ToField HyperdataDocument where
  toField = toJSONField

instance Arbitrary HyperdataDocument where
    arbitrary = elements arbitraryHyperdataDocuments

arbitraryHyperdataDocuments :: [HyperdataDocument]
arbitraryHyperdataDocuments =
  map toHyperdataDocument' ([ ("AI is big but less than crypto", "Troll System journal")
                            , ("Crypto is big but less than AI", "System Troll review" )
                            , ("Science is magic"              , "Closed Source review")
                            , ("Open science for all"          , "No Time"             )
                            , ("Closed science for me"         , "No Space"            )
                            ] :: [(Text, Text)])
  where
    toHyperdataDocument' (t1,t2) =
      HyperdataDocument Nothing Nothing Nothing Nothing Nothing Nothing (Just t1)
                      Nothing Nothing (Just t2) Nothing Nothing Nothing Nothing Nothing
                      Nothing Nothing Nothing   Nothing

data HyperdataField a =
  HyperdataField { _hf_type :: !CodeType
                 , _hf_name :: !Text
                 , _hf_data :: !a
                 } deriving (Generic)
$(deriveJSON (unPrefix "_hf_") ''HyperdataField)
$(makeLenses ''HyperdataField)

defaultHyperdataField :: HyperdataField CorpusField
defaultHyperdataField = HyperdataField Markdown "name" defaultCorpusField

instance (ToSchema a) => ToSchema (HyperdataField a) where
  declareNamedSchema =
    genericDeclareNamedSchema (unPrefixSwagger "_hf_")
    -- & mapped.schema.description ?~ "HyperdataField"
    -- & mapped.schema.example ?~ toJSON defaultHyperdataField

------------------------------------------------------------------------
data HyperdataCorpus =
  HyperdataCorpus { _hc_fields :: ![HyperdataField CorpusField] }
    deriving (Generic)
$(deriveJSON (unPrefix "_hc_") ''HyperdataCorpus)
$(makeLenses ''HyperdataCorpus)

instance Hyperdata HyperdataCorpus

------------------------------------------------------------------------
------------------------------------------------------------------------
docExample :: ByteString
docExample = "{\"doi\":\"sdfds\",\"publication_day\":6,\"language_iso2\":\"en\",\"publication_minute\":0,\"publication_month\":7,\"language_iso3\":\"eng\",\"publication_second\":0,\"authors\":\"Nils Hovdenak, Kjell Haram\",\"publication_year\":2012,\"publication_date\":\"2012-07-06 00:00:00+00:00\",\"language_name\":\"English\",\"realdate_full_\":\"2012 01 12\",\"source\":\"European journal of obstetrics, gynecology, and reproductive biology\",\"abstract\":\"The literature was searched for publications on minerals and vitamins during pregnancy and the possible influence of supplements on pregnancy outcome.\",\"title\":\"Influence of mineral and vitamin supplements on pregnancy outcome.\",\"publication_hour\":0}"

corpusExample :: ByteString
corpusExample = "" -- TODO

defaultCorpus :: HyperdataCorpus
defaultCorpus = HyperdataCorpus [
    HyperdataField JSON "Mandatory fields" (JsonField "Title" "Descr" "Bool query" "Authors")
  , HyperdataField Markdown "Optional Text" (MarkdownField "# title\n## subtitle")
  ]

hyperdataCorpus :: HyperdataCorpus
hyperdataCorpus = case decode corpusExample of
  Just hp -> hp
  Nothing -> defaultCorpus

instance Arbitrary HyperdataCorpus where
    arbitrary = pure hyperdataCorpus -- TODO

------------------------------------------------------------------------
data HyperdataList =
  HyperdataList { hd_chart :: !(Maybe (ChartMetrics Histo))
                , hd_list :: !(Maybe Text)
                , hd_pie :: !(Maybe (ChartMetrics Histo))
                , hd_scatter :: !(Maybe Metrics)
                , hd_tree :: !(Maybe (ChartMetrics [MyTree]))
                } deriving (Show, Generic)
$(deriveJSON (unPrefix "hd_") ''HyperdataList)

instance Hyperdata HyperdataList

------------------------------------------------------------------------
data HyperdataAnnuaire = HyperdataAnnuaire { hyperdataAnnuaire_title        :: !(Maybe Text)
                                           , hyperdataAnnuaire_desc         :: !(Maybe Text)
                                           } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataAnnuaire_") ''HyperdataAnnuaire)

instance Hyperdata HyperdataAnnuaire

hyperdataAnnuaire :: HyperdataAnnuaire
hyperdataAnnuaire = HyperdataAnnuaire (Just "Annuaire Title") (Just "Annuaire Description")

instance Arbitrary HyperdataAnnuaire where
    arbitrary = pure hyperdataAnnuaire -- TODO

------------------------------------------------------------------------
newtype HyperdataAny = HyperdataAny Object
  deriving (Show, Generic, ToJSON, FromJSON)

instance Hyperdata HyperdataAny

instance Arbitrary HyperdataAny where
    arbitrary = pure $ HyperdataAny mempty -- TODO produce arbitrary objects
------------------------------------------------------------------------

{-
instance Arbitrary HyperdataList' where
  arbitrary = elements [HyperdataList' (Just "from list A")]
-}

                      ----
data HyperdataListModel =
  HyperdataListModel { _hlm_params  :: !(Int, Int)
                     , _hlm_path    :: !Text
                     , _hlm_score   :: !(Maybe Double)
                     } deriving (Show, Generic)

instance Hyperdata HyperdataListModel
instance Arbitrary HyperdataListModel where
  arbitrary = elements [HyperdataListModel (100,100) "models/example.model" Nothing]

$(deriveJSON (unPrefix "_hlm_") ''HyperdataListModel)
$(makeLenses ''HyperdataListModel)

------------------------------------------------------------------------
data HyperdataScore = HyperdataScore { hyperdataScore_preferences   :: !(Maybe Text)
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataScore_") ''HyperdataScore)

instance Hyperdata HyperdataScore

------------------------------------------------------------------------
data HyperdataResource = HyperdataResource { hyperdataResource_preferences   :: !(Maybe Text)
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataResource_") ''HyperdataResource)

instance Hyperdata HyperdataResource

------------------------------------------------------------------------
data HyperdataDashboard = HyperdataDashboard { hyperdataDashboard_preferences   :: !(Maybe Text)
                                             , hyperdataDashboard_charts        :: ![Chart]
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataDashboard_") ''HyperdataDashboard)

instance Hyperdata HyperdataDashboard

------------------------------------------------------------------------
-- TODO add the Graph Structure here
data HyperdataPhylo = HyperdataPhylo { hyperdataPhylo_preferences   :: !(Maybe Text)
                                     , hyperdataPhylo_data          :: !(Maybe Phylo)
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataPhylo_") ''HyperdataPhylo)

instance Hyperdata HyperdataPhylo

------------------------------------------------------------------------
-- | TODO FEATURE: Notebook saved in the node
data HyperdataNotebook = HyperdataNotebook { hyperdataNotebook_preferences   :: !(Maybe Text)
                                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataNotebook_") ''HyperdataNotebook)

instance Hyperdata HyperdataNotebook


-- | TODO CLEAN
data HyperData = HyperdataTexts { hd_preferences :: Maybe Text }
               | HyperdataList' { hd_preferences :: Maybe Text}
  deriving (Show, Generic)

$(deriveJSON (unPrefix "hd_") ''HyperData)

instance Hyperdata HyperData

------------------------------------------------------------------------

hyperdataDocument :: HyperdataDocument
hyperdataDocument = case decode docExample of
                      Just hp -> hp
                      Nothing -> HyperdataDocument Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing Nothing
                                                   Nothing Nothing Nothing


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance ToSchema HyperdataCorpus where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hc_") proxy
    & mapped.schema.description ?~ "Corpus"
    & mapped.schema.example ?~ toJSON hyperdataCorpus

instance ToSchema HyperdataAnnuaire where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "hyperdataAnnuaire_") proxy
    & mapped.schema.description ?~ "an annuaire"
    & mapped.schema.example ?~ toJSON hyperdataAnnuaire

instance ToSchema HyperdataDocument where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hyperdataDocument_") proxy
    & mapped.schema.description ?~ "a document"
    & mapped.schema.example ?~ toJSON hyperdataDocument

instance ToSchema HyperdataAny where
  declareNamedSchema proxy =
    pure $ genericNameSchema defaultSchemaOptions proxy mempty
             & schema.description ?~ "a node"
             & schema.example ?~ emptyObject -- TODO

------------------------------------------------------------------------

instance FromField HyperdataAny where
    fromField = fromField'

instance FromField HyperdataCorpus
  where
    fromField = fromField'

instance FromField HyperdataDocument
  where
    fromField = fromField'

instance FromField HyperdataDocumentV3
  where
    fromField = fromField'

instance FromField HyperData
  where
    fromField = fromField'

instance FromField HyperdataListModel
  where
    fromField = fromField'

instance FromField HyperdataPhylo
  where
    fromField = fromField'

instance FromField HyperdataAnnuaire
  where
    fromField = fromField'

instance FromField HyperdataList
  where
    fromField = fromField'

------------------------------------------------------------------------

instance QueryRunnerColumnDefault PGJsonb HyperdataAny
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataList
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperData
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocument
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocumentV3
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataListModel
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataPhylo
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataAnnuaire
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
