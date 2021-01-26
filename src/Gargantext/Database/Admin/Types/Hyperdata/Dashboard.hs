{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Dashboard
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

module Gargantext.Database.Admin.Types.Hyperdata.Dashboard
  where

import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.CorpusField

------------------------------------------------------------------------

data HyperdataDashboard =
  HyperdataDashboard { _hd_preferences :: !(Maybe Text)
                     , _hd_charts      :: ![Chart]
                     , _hd_fields      :: ![HyperdataField CorpusField]
                     }
  deriving (Show, Generic)


defaultHyperdataDashboard :: HyperdataDashboard
defaultHyperdataDashboard = HyperdataDashboard Nothing [] defaultHyperdataDashboardFields

defaultHyperdataDashboardFields :: [HyperdataField CorpusField]
defaultHyperdataDashboardFields =
  [ HyperdataField Markdown
                   "Corpus analysis"
                   (MarkdownField "# title\n## subtitle")

  , HyperdataField JSON
                  "Metadata (Experts only)"
                  (JsonField "Title" "Descr" "Bool query" "Authors")
  ]

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Hyperdata HyperdataDashboard

$(makeLenses ''HyperdataDashboard)
$(deriveJSON (unPrefix "_hd_") ''HyperdataDashboard)

instance Arbitrary HyperdataDashboard where
    arbitrary = pure defaultHyperdataDashboard

instance ToSchema HyperdataDashboard where
  declareNamedSchema proxy =
    pure $ genericNameSchema defaultSchemaOptions proxy mempty
    -- genericDeclareNamedSchema (unPrefixSwagger "hp_") proxy
             & schema.description ?~ "Dashboard Hyperdata"
             & schema.example ?~ toJSON defaultHyperdataDashboard

instance FromField HyperdataDashboard where
    fromField = fromField'

instance QueryRunnerColumnDefault PGJsonb HyperdataDashboard
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

