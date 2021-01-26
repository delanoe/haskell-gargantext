{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.CorpusField
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gargantext.Database.Admin.Types.Hyperdata.CorpusField
where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude


data CodeType = JSON | Markdown | Haskell | Python
  deriving (Generic, Show, Eq)
instance ToJSON CodeType
instance FromJSON CodeType
instance ToSchema CodeType


------------------------------------------------------------------------
data CorpusField = MarkdownField { _cf_text    :: !Text }
                 | HaskellField { _cf_haskell :: !Text }
                 | PythonField  { _cf_python  :: !Text }
                 | JsonField    { _cf_title   :: !Text
                                 , _cf_desc    :: !Text
                                 , _cf_query   :: !Text
                                 , _cf_authors :: !Text
                                 -- , _cf_resources :: ![Resource]
                                 }
                  deriving (Show, Generic)

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
                 } deriving (Generic, Show)
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
