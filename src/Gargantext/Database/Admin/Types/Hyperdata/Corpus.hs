{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Corpus
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


module Gargantext.Database.Admin.Types.Hyperdata.Corpus
  where

import Gargantext.API.Ngrams.NTree (MyTree)
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Database.Admin.Types.Metrics (ChartMetrics(..), Metrics)
import Gargantext.Viz.Phylo (Phylo(..))
import Gargantext.Viz.Types (Histo(..))
import Protolude hiding (ByteString)


data CodeType = JSON | Markdown | Haskell
  deriving (Generic, Eq)
instance ToJSON CodeType
instance FromJSON CodeType
instance ToSchema CodeType

------------------------------------------------------------------------
------------------------------------------------------------------------
data CorpusField = MarkdownField { _cf_text :: !Text }
                  | JsonField { _cf_title   :: !Text
                              , _cf_desc    :: !Text
                              , _cf_query   :: !Text
                              , _cf_authors :: !Text
                              -- , _cf_resources :: ![Resource]
                              }
                  | HaskellField { _cf_haskell :: !Text }
                  deriving (Generic)

isField :: CodeType -> CorpusField -> Bool
isField Markdown (MarkdownField   _) = True
isField JSON     (JsonField _ _ _ _) = True
isField Haskell  (HaskellField    _) = True
isField _ _                          = False

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

data HyperdataField a =
  HyperdataField { _hf_type :: !CodeType
                 , _hf_name :: !Text
                 , _hf_data :: !a
                 } deriving (Generic)
$(deriveJSON (unPrefix "_hf_") ''HyperdataField)
$(makeLenses ''HyperdataField)

defaultHyperdataField :: HyperdataField CorpusField
defaultHyperdataField = HyperdataField Markdown "name" defaultCorpusField

instance (Typeable a, ToSchema a) => ToSchema (HyperdataField a) where
  declareNamedSchema =
    wellNamedSchema "_hf_"
    -- & mapped.schema.description ?~ "HyperdataField"
    -- & mapped.schema.example ?~ toJSON defaultHyperdataField

------------------------------------------------------------------------
data HyperdataCorpus =
  HyperdataCorpus { _hc_fields :: ![HyperdataField CorpusField] }
    deriving (Generic)
$(deriveJSON (unPrefix "_hc_") ''HyperdataCorpus)
$(makeLenses ''HyperdataCorpus)

instance Hyperdata HyperdataCorpus

type HyperdataFolder = HyperdataCorpus
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

instance Arbitrary HyperdataCorpus where
    arbitrary = pure hyperdataCorpus -- TODO

------------------------------------------------------------------------
data HyperdataList =
  HyperdataList { hd_chart   :: !(Maybe (ChartMetrics Histo))
                , hd_list    :: !(Maybe Text)
                , hd_pie     :: !(Maybe (ChartMetrics Histo))
                , hd_scatter :: !(Maybe Metrics)
                , hd_tree    :: !(Maybe (ChartMetrics [MyTree]))
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
------------------------------------------------------------------------
-- TODO add the Graph Structure here

------------------------------------------------------------------------
-- | TODO CLEAN
-- | TODO FEATURE: Notebook saved in the node
data HyperData = HyperdataTexts { hd_preferences :: !(Maybe Text)}
               | HyperdataList' { hd_preferences :: !(Maybe Text)}
               | HyperdataDashboard { hd_preferences :: !(Maybe Text)
                                    , hd_charts      :: ![Chart]
                                    }
               | HyperdataNotebook { hd_preferences :: !(Maybe Text)}
               | HyperdataPhylo    { hd_preferences :: !(Maybe Text)
                                   , hd_data        :: !(Maybe Phylo)
                                   }

  deriving (Show, Generic)

$(deriveJSON (unPrefix "hd_") ''HyperData)
instance Hyperdata HyperData

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

instance FromField HyperData
  where
    fromField = fromField'

instance FromField HyperdataListModel
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

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataListModel
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataAnnuaire
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
