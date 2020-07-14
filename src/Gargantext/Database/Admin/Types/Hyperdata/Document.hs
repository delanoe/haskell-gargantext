{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Document
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Gargantext.Database.Admin.Types.Hyperdata.Document where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude


data StatusV3  = StatusV3 { statusV3_error  :: !(Maybe Text)
                          , statusV3_action :: !(Maybe Text)
                      } deriving (Show, Generic)
$(deriveJSON (unPrefix "statusV3_") ''StatusV3)



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

------------------------------------------------------------------------
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
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataDocument
instance Hyperdata HyperdataDocumentV3
------------------------------------------------------------------------
instance ToSchema HyperdataDocument where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hyperdataDocument_") proxy
    & mapped.schema.description ?~ "a document"
    & mapped.schema.example ?~ toJSON defaultHyperdataDocument
------------------------------------------------------------------------
instance FromField HyperdataDocument
  where
    fromField = fromField'

instance FromField HyperdataDocumentV3
  where
    fromField = fromField'

instance ToField HyperdataDocument where
  toField = toJSONField
------------------------------------------------------------------------
instance QueryRunnerColumnDefault PGJsonb HyperdataDocument
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocumentV3
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

------------------------------------------------------------------------
$(deriveJSON (unPrefix "hyperdataDocumentV3_") ''HyperdataDocumentV3)
$(deriveJSON (unPrefix "_hyperdataDocument_") ''HyperdataDocument)
------------------------------------------------------------------------
$(makeLenses ''HyperdataDocument)

------------------------------------------------------------------------
class ToHyperdataDocument a where
  toHyperdataDocument :: a -> HyperdataDocument

instance ToHyperdataDocument HyperdataDocument
  where
    toHyperdataDocument = identity

------------------------------------------------------------------------
instance Eq HyperdataDocument where
  (==) h1 h2 = (==) (_hyperdataDocument_uniqId h1) (_hyperdataDocument_uniqId h2)

------------------------------------------------------------------------
instance Ord HyperdataDocument where
  compare h1 h2 = compare (_hyperdataDocument_publication_date h1) (_hyperdataDocument_publication_date h2)
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

