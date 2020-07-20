{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Corpus
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

module Gargantext.Database.Admin.Types.Hyperdata.Corpus
  where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude


data CodeType = JSON | Markdown | Haskell
  deriving (Generic, Eq)
instance ToJSON CodeType
instance FromJSON CodeType
instance ToSchema CodeType

------------------------------------------------------------------------
data CorpusField = MarkdownField { _cf_text    :: !Text }
                  | HaskellField { _cf_haskell :: !Text }
                  | JsonField    { _cf_title   :: !Text
                                 , _cf_desc    :: !Text
                                 , _cf_query   :: !Text
                                 , _cf_authors :: !Text
                                 -- , _cf_resources :: ![Resource]
                                 }
                  deriving (Generic)

defaultCorpusField :: CorpusField
defaultCorpusField = MarkdownField "# Title"

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
$(makeLenses ''CorpusField)
$(deriveJSON (unPrefix "_cf_") ''CorpusField)

instance ToSchema CorpusField where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_cf_") proxy
    & mapped.schema.description ?~ "CorpusField"
    & mapped.schema.example ?~ toJSON defaultCorpusField

------------------------------------------------------------------------
data HyperdataField a =
  HyperdataField { _hf_type :: !CodeType
                 , _hf_name :: !Text
                 , _hf_data :: !a
                 } deriving (Generic)
defaultHyperdataField :: HyperdataField CorpusField
defaultHyperdataField = HyperdataField Markdown "name" defaultCorpusField

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
$(makeLenses ''HyperdataField)
$(deriveJSON (unPrefix "_hf_") ''HyperdataField)

instance (Typeable a, ToSchema a) => ToSchema (HyperdataField a) where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hf_") proxy
    & mapped.schema.description ?~ "Hyperdata Field"
    & mapped.schema.example ?~ toJSON defaultCorpusField
{-
  declareNamedSchema =
    wellNamedSchema "_hf_"
    -- & mapped.schema.description ?~ "HyperdataField"
    -- & mapped.schema.example ?~ toJSON defaultHyperdataField
-}

------------------------------------------------------------------------
data HyperdataCorpus =
  HyperdataCorpus { _hc_fields :: ![HyperdataField CorpusField] }
    deriving (Generic)

defaultHyperdataCorpus :: HyperdataCorpus
defaultHyperdataCorpus =
  HyperdataCorpus [ HyperdataField JSON
                                   "Mandatory fields"
                                   (JsonField "Title" "Descr" "Bool query" "Authors")
                  , HyperdataField Markdown
                                   "Optional Text"
                                   (MarkdownField "# title\n## subtitle")
                  ]

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Annuaire and Corpus should be the same
data HyperdataAnnuaire = HyperdataAnnuaire { _ha_title        :: !(Maybe Text)
                                           , _ha_desc         :: !(Maybe Text)
                                           } deriving (Show, Generic)

defaultHyperdataAnnuaire :: HyperdataAnnuaire
defaultHyperdataAnnuaire = HyperdataAnnuaire (Just "Title") (Just "Description")

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataCorpus
instance Hyperdata HyperdataAnnuaire

$(makeLenses ''HyperdataCorpus)
$(makeLenses ''HyperdataAnnuaire)

$(deriveJSON (unPrefix "_hc_") ''HyperdataCorpus)
$(deriveJSON (unPrefix "_ha_") ''HyperdataAnnuaire)

------------------------------------------------------------------------
instance ToSchema HyperdataCorpus where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hc_") proxy
    & mapped.schema.description ?~ "Corpus Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataCorpus

instance ToSchema HyperdataAnnuaire where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_ha_") proxy
    & mapped.schema.description ?~ "Annuaire Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataAnnuaire
------------------------------------------------------------------------
instance Arbitrary HyperdataCorpus where
    arbitrary = pure defaultHyperdataCorpus

instance Arbitrary HyperdataAnnuaire where
  arbitrary = pure defaultHyperdataAnnuaire
------------------------------------------------------------------------
instance FromField HyperdataCorpus
  where
    fromField = fromField'

instance FromField HyperdataAnnuaire
  where
    fromField = fromField'
------------------------------------------------------------------------
instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataAnnuaire
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
