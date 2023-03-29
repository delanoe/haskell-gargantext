{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TypeOperators       #-}

module Gargantext.API.Node.FrameCalcUpload where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSU8
import Data.Swagger
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
import Web.FormUrlEncoded (FromForm)

import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Job (jobLogInit, jobLogSuccess, jobLogFail)
import Gargantext.API.Node.Corpus.New (addToCorpusWithForm)
import Gargantext.API.Node.Corpus.New.Types (FileFormat(..), FileType(..))
import Gargantext.API.Node.Types (NewWithForm(..))
import Gargantext.API.Prelude
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Text.List.Social (FlowSocialListWith(..))
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Admin.Types.Hyperdata.Frame
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (HasConfig)
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType, getNodeWith)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI, jobHandleLogger)
import Gargantext.Core (Lang)

data FrameCalcUpload = FrameCalcUpload {
  _wf_lang      :: !(Maybe Lang)
, _wf_selection :: !FlowSocialListWith
}
  deriving (Generic)

instance FromForm FrameCalcUpload
instance FromJSON FrameCalcUpload
instance ToJSON FrameCalcUpload
instance ToSchema FrameCalcUpload

type API = Summary " FrameCalc upload"
           :> "add"
           :> "framecalc"
           :> "async"
           :> AsyncJobs JobLog '[JSON] FrameCalcUpload JobLog

api :: UserId -> NodeId -> ServerT API (GargM Env GargError)
api uId nId =
  serveJobsAPI UploadFrameCalcJob $ \jHandle p ->
    frameCalcUploadAsync uId nId p (jobHandleLogger jHandle) (jobLogInit 5)



frameCalcUploadAsync :: (HasConfig env, FlowCmdM env err m)
                     => UserId
                     -> NodeId
                     -> FrameCalcUpload
                     -> (JobLog -> m ())
                     -> JobLog
                     -> m JobLog
frameCalcUploadAsync uId nId (FrameCalcUpload _wf_lang _wf_selection) logStatus jobLog = do
  logStatus jobLog

  -- printDebug "[frameCalcUploadAsync] uId" uId
  -- printDebug "[frameCalcUploadAsync] nId" nId

  node <- getNodeWith nId (Proxy :: Proxy HyperdataFrame)
  let (HyperdataFrame { _hf_base = base
                      , _hf_frame_id = frame_id }) = node ^. node_hyperdata

  let csvUrl = base <> "/" <> frame_id <> ".csv"
  -- printDebug "[frameCalcUploadAsync] csvUrl" csvUrl

  res <- liftBase $ do
    manager <- newManager tlsManagerSettings
    req <- parseRequest $ T.unpack csvUrl
    httpLbs req manager
  let body = T.pack $ BSU8.toString $ BSL.toStrict $ responseBody res

  -- printDebug "body" body
  mCId <- getClosestParentIdByType nId NodeCorpus
  -- printDebug "[frameCalcUploadAsync] mCId" mCId

  jobLog2 <- case mCId of
    Nothing -> pure $ jobLogFail jobLog
    Just cId ->
      addToCorpusWithForm (RootId (NodeId uId)) cId (NewWithForm CSV Plain body _wf_lang "calc-upload.csv" _wf_selection) logStatus jobLog

  pure $ jobLogSuccess jobLog2
