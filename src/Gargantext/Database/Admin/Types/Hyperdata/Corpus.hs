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
------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataCorpus
$(deriveJSON (unPrefix "_hc_") ''HyperdataCorpus)
$(makeLenses ''HyperdataCorpus)



------------------------------------------------------------------------
data HyperdataFrame =
  HyperdataFrame { base :: !Text 
                 , frame_id :: !Text
                 }
    deriving (Generic)
$(deriveJSON (unPrefix "") ''HyperdataFrame)
$(makeLenses ''HyperdataFrame)

instance Hyperdata HyperdataFrame

------------------------------------------------------------------------
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

defaultHyperdataCorpus :: HyperdataCorpus
defaultHyperdataCorpus = defaultCorpus

instance Arbitrary HyperdataCorpus where
    arbitrary = pure hyperdataCorpus -- TODO

------------------------------------------------------------------------
------------------------------------------------------------------------
data HyperdataAnnuaire = HyperdataAnnuaire { hyperdataAnnuaire_title        :: !(Maybe Text)
                                           , hyperdataAnnuaire_desc         :: !(Maybe Text)
                                           } deriving (Show, Generic)
$(deriveJSON (unPrefix "hyperdataAnnuaire_") ''HyperdataAnnuaire)

instance Hyperdata HyperdataAnnuaire

defaultHyperdataAnnuaire :: HyperdataAnnuaire
defaultHyperdataAnnuaire = HyperdataAnnuaire (Just "Title") (Just "Description")

instance Arbitrary HyperdataAnnuaire where
  arbitrary = pure defaultHyperdataAnnuaire -- TODO

------------------------------------------------------------------------
------------------------------------------------------------------------
data HyperdataNotebook =
  HyperdataNotebook { hn_preferences :: !(Maybe Text)}
  deriving (Show, Generic)


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
    & mapped.schema.example ?~ toJSON defaultHyperdataAnnuaire

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
