{-|
Module      : Gargantext.API.Node.Corpus.Annuaire
Description : New annuaire API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

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
import Servant.Job.Core
import Servant.Job.Types
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm)
import qualified Gargantext.API.Node.Corpus.New.File as NewFile


type Api = Summary "New Annuaire endpoint"
         :> Post '[JSON] AnnuaireId

------------------------------------------------------------------------
------------------------------------------------------------------------
data AnnuaireWithForm = AnnuaireWithForm
  { _wf_filetype :: !NewFile.FileType
  , _wf_data     :: !Text
  , _wf_lang     :: !(Maybe Lang)
  } deriving (Eq, Show, Generic)

makeLenses ''AnnuaireWithForm
instance FromForm AnnuaireWithForm
instance FromJSON AnnuaireWithForm where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToSchema AnnuaireWithForm where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")

------------------------------------------------------------------------
type AsyncJobs event ctI input output =
  AsyncJobsAPI' 'Unsafe 'Safe ctI '[JSON] Maybe event input output
------------------------------------------------------------------------

type AddWithForm = Summary "Add with FormUrlEncoded to annuaire endpoint"
   :> "annuaire"
   :> Capture "annuaire_id" AnnuaireId
   :> "add"
   :> "form"
   :> "async"
   :> AsyncJobs JobLog '[FormUrlEncoded] AnnuaireWithForm JobLog

------------------------------------------------------------------------
addToAnnuaireWithForm :: FlowCmdM env err m
                    => AnnuaireId
                    -> AnnuaireWithForm
                    -> (JobLog -> m ())
                    -> m JobLog
addToAnnuaireWithForm _cid (AnnuaireWithForm ft _d _l) logStatus = do

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

  logStatus JobLog { _scst_succeeded = Just 1
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 1
                          , _scst_events    = Just []
                          }
  -- cid' <- flowCorpus "user1"
  --                    (Right [cid])
  --                    (Multi $ fromMaybe EN l)
  --                    (map (map toHyperdataDocument) docs)

  -- printDebug "cid'" cid'

  pure      JobLog { _scst_succeeded = Just 2
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }

