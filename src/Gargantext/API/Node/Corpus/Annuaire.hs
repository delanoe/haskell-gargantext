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
import Servant
import Servant.Job.Core
import Servant.Job.Types
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm)

import qualified Gargantext.API.Node.Corpus.New.Types as NewTypes
import Gargantext.API.Admin.Orchestrator.Types hiding (AsyncJobs)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)  -- flowAnnuaire
import Gargantext.Database.Admin.Types.Node (AnnuaireId)
import Gargantext.Prelude


type Api = Summary "New Annuaire endpoint"
         :> Post '[JSON] AnnuaireId

------------------------------------------------------------------------
------------------------------------------------------------------------
data AnnuaireWithForm = AnnuaireWithForm
  { _wf_filetype :: !NewTypes.FileType
  , _wf_data     :: !Text
  , _wf_lang     :: !(Maybe Lang)
  } deriving (Eq, Show, Generic)

makeLenses ''AnnuaireWithForm
instance FromForm AnnuaireWithForm
instance FromJSON AnnuaireWithForm where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToJSON AnnuaireWithForm where
  toJSON = genericToJSON $ jsonOptions "_wf_"

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
addToAnnuaireWithForm _cid (AnnuaireWithForm { _wf_filetype }) logStatus = do

  printDebug "ft" _wf_filetype

  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
  pure      JobLog { _scst_succeeded = Just 2
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 0
                   , _scst_events    = Just []
                   }
