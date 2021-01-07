{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Document
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gargantext.Database.Admin.Types.Hyperdata.Document where

import Data.Maybe (catMaybes)
import Gargantext.Prelude
import Gargantext.Core.Text (HasText(..))
import Gargantext.Core.Utils.Prefix (unCapitalize, dropPrefix)
import Gargantext.Database.Admin.Types.Hyperdata.Prelude

------------------------------------------------------------------------
data HyperdataDocument = HyperdataDocument { _hd_bdd                :: !(Maybe Text)
                                           , _hd_doi                :: !(Maybe Text)
                                           , _hd_url                :: !(Maybe Text)
                                           , _hd_uniqId             :: !(Maybe Text)
                                           , _hd_uniqIdBdd          :: !(Maybe Text)
                                           , _hd_page               :: !(Maybe Int)
                                           , _hd_title              :: !(Maybe Text)
                                           , _hd_authors            :: !(Maybe Text)
                                           , _hd_institutes         :: !(Maybe Text)
                                           , _hd_source             :: !(Maybe Text)
                                           , _hd_abstract           :: !(Maybe Text)
                                           , _hd_publication_date   :: !(Maybe Text)
                                           , _hd_publication_year   :: !(Maybe Int)
                                           , _hd_publication_month  :: !(Maybe Int)
                                           , _hd_publication_day    :: !(Maybe Int)
                                           , _hd_publication_hour   :: !(Maybe Int)
                                           , _hd_publication_minute :: !(Maybe Int)
                                           , _hd_publication_second :: !(Maybe Int)
                                           , _hd_language_iso2      :: !(Maybe Text)
                                           }
  deriving (Show, Generic)


instance HasText HyperdataDocument
  where
    hasText h = catMaybes [ _hd_title    h
                          , _hd_abstract h
                          ]

defaultHyperdataDocument :: HyperdataDocument
defaultHyperdataDocument = case decode docExample of
  Just hp -> hp
  Nothing -> HyperdataDocument Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing

  where
    docExample :: ByteString
    docExample = "{\"doi\":\"sdfds\",\"publication_day\":6,\"language_iso2\":\"en\",\"publication_minute\":0,\"publication_month\":7,\"language_iso3\":\"eng\",\"publication_second\":0,\"authors\":\"Nils Hovdenak, Kjell Haram\",\"publication_year\":2012,\"publication_date\":\"2012-07-06 00:00:00+00:00\",\"language_name\":\"English\",\"realdate_full_\":\"2012 01 12\",\"source\":\"European journal of obstetrics, gynecology, and reproductive biology\",\"abstract\":\"The literature was searched for publications on minerals and vitamins during pregnancy and the possible influence of supplements on pregnancy outcome.\",\"title\":\"Influence of mineral and vitamin supplements on pregnancy outcome.\",\"publication_hour\":0}"

------------------------------------------------------------------------
-- | Legacy Garg V3 compatibility (to be removed one year after release)
data StatusV3  = StatusV3 { statusV3_error  :: !(Maybe Text)
                          , statusV3_action :: !(Maybe Text)
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "statusV3_") ''StatusV3)


------------------------------------------------------------------------
data HyperdataDocumentV3 = HyperdataDocumentV3 { _hdv3_publication_day    :: !(Maybe Int)
                                               , _hdv3_language_iso2      :: !(Maybe Text)
                                               , _hdv3_publication_second :: !(Maybe Int)
                                               , _hdv3_publication_minute :: !(Maybe Int)
                                               , _hdv3_publication_month  :: !(Maybe Int)
                                               , _hdv3_publication_hour   :: !(Maybe Int)
                                               , _hdv3_error              :: !(Maybe Text)
                                               , _hdv3_language_iso3      :: !(Maybe Text)
                                               , _hdv3_authors            :: !(Maybe Text)
                                               , _hdv3_publication_year   :: !(Maybe Int)
                                               , _hdv3_publication_date   :: !(Maybe Text)
                                               , _hdv3_language_name      :: !(Maybe Text)
                                               , _hdv3_statuses           :: !(Maybe [StatusV3])
                                               , _hdv3_realdate_full_     :: !(Maybe Text)
                                               , _hdv3_source             :: !(Maybe Text)
                                               , _hdv3_abstract           :: !(Maybe Text)
                                               , _hdv3_title              :: !(Maybe Text)
                                               } deriving (Show, Generic)


------------------------------------------------------------------------
-- | Instances for Analysis
------------------------------------------------------------------------
class ToHyperdataDocument a where
  toHyperdataDocument :: a -> HyperdataDocument

instance ToHyperdataDocument HyperdataDocument
  where
    toHyperdataDocument = identity

------------------------------------------------------------------------
instance Eq HyperdataDocument where
  (==) h1 h2 = (==) (_hd_uniqId h1) (_hd_uniqId h2)

------------------------------------------------------------------------
instance Ord HyperdataDocument where
  compare h1 h2 = compare (_hd_publication_date h1) (_hd_publication_date h2)
------------------------------------------------------------------------
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



------------------------------------------------------------------------
-- | Common Instances of Hyperdata
------------------------------------------------------------------------
instance Hyperdata HyperdataDocument
instance Hyperdata HyperdataDocumentV3
------------------------------------------------------------------------
$(makeLenses ''HyperdataDocument)
makePrisms ''HyperdataDocument

$(makeLenses ''HyperdataDocumentV3)

-- $(deriveJSON (unPrefix "_hd_") ''HyperdataDocument)

instance FromJSON HyperdataDocument
  where
    parseJSON = genericParseJSON
            ( defaultOptions { sumEncoding = ObjectWithSingleField 
                            , fieldLabelModifier = unCapitalize . dropPrefix "_hd_"
                            , omitNothingFields = True
                            }
            )

instance ToJSON HyperdataDocument
  where
    toJSON = genericToJSON
           ( defaultOptions { sumEncoding = ObjectWithSingleField 
                            , fieldLabelModifier = unCapitalize . dropPrefix "_hd_"
                            , omitNothingFields = True
                            }
           )



$(deriveJSON (unPrefix "_hdv3_") ''HyperdataDocumentV3)

instance ToSchema HyperdataDocument where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hd_") proxy
    & mapped.schema.description ?~ "Document Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataDocument

{-
-- | For now HyperdataDocumentV3 is not exposed with the API
instance ToSchema HyperdataDocumentV3 where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "hyperdataDocumentV3_") proxy
    & mapped.schema.description ?~ "Document Hyperdata for Garg V3"
    & mapped.schema.example ?~ toJSON defaultHyperdataDocumentV3
-}

------------------------------------------------------------------------
instance FromField HyperdataDocument
  where
    fromField = fromField'

instance FromField HyperdataDocumentV3
  where
    fromField = fromField'

-------
instance ToField HyperdataDocument where
  toField = toJSONField

instance ToField HyperdataDocumentV3 where
  toField = toJSONField

------------------------------------------------------------------------
instance QueryRunnerColumnDefault PGJsonb HyperdataDocument
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocumentV3
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
------------------------------------------------------------------------
