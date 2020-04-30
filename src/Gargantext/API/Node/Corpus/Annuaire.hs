{-|
Module      : Gargantext.API.Node.Corpus.Annuaire
Description : New annuaire API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}

module Gargantext.API.Node.Corpus.Annuaire
      where

import Control.Lens hiding (elements)
import Data.Aeson
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.Core (Lang(..))
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.Action.Flow (FlowCmdM)  -- flowAnnuaire
import Gargantext.Database.Admin.Types.Node (AnnuaireId)
import Gargantext.Prelude
import Servant
import Servant.API.Flatten (Flat)
import Servant.Job.Core
import Servant.Job.Types
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm)
import qualified Gargantext.API.Node.Corpus.New.File as NewFile


type Api = Summary "New Annuaire endpoint"
         :> Post '[JSON] AnnuaireId

------------------------------------------------------------------------
------------------------------------------------------------------------
data WithForm = WithForm
  { _wf_filetype :: !NewFile.FileType
  , _wf_data     :: !Text
  , _wf_lang     :: !(Maybe Lang)
  } deriving (Eq, Show, Generic)

makeLenses ''WithForm
instance FromForm WithForm
instance FromJSON WithForm where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToSchema WithForm where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")

------------------------------------------------------------------------
type AsyncJobs event ctI input output =
  Flat (AsyncJobsAPI' 'Unsafe 'Safe ctI '[JSON] Maybe event input output)
------------------------------------------------------------------------

type AddWithForm = Summary "Add with FormUrlEncoded to annuaire endpoint"
   :> "annuaire"
   :> Capture "annuaire_id" AnnuaireId
   :> "add"
   :> "form"
   :> "async"
   :> AsyncJobs ScraperStatus '[FormUrlEncoded] WithForm ScraperStatus

------------------------------------------------------------------------
addToAnnuaireWithForm :: FlowCmdM env err m
                    => AnnuaireId
                    -> WithForm
                    -> (ScraperStatus -> m ())
                    -> m ScraperStatus
addToAnnuaireWithForm _cid (WithForm ft _d _l) logStatus = do

  printDebug "ft" ft

  -- let
    -- parse = case ft of
    --   CSV_HAL   -> Parser.parseFormat Parser.CsvHal
    --   CSV       -> Parser.parseFormat Parser.CsvGargV3
    --   WOS       -> Parser.parseFormat Parser.WOS
    --   PresseRIS -> Parser.parseFormat Parser.RisPresse

  -- docs <- liftBase
  --       $ splitEvery 500
  --      <$> take 1000000
  --      <$> parse (cs d)

  logStatus ScraperStatus { _scst_succeeded = Just 1
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 1
                          , _scst_events    = Just []
                          }
  -- cid' <- flowCorpus "user1"
  --                    (Right [cid])
  --                    (Multi $ fromMaybe EN l)
  --                    (map (map toHyperdataDocument) docs)

  -- printDebug "cid'" cid'

  pure      ScraperStatus { _scst_succeeded = Just 2
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }

