{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TypeOperators       #-}

module Gargantext.API.Node.FrameCalcUpload where

import Control.Lens ((^.))
import Data.Aeson
import Data.Swagger
import GHC.Generics (Generic)
import Servant
import Servant.Job.Async
import Web.FormUrlEncoded (FromForm)

import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Prelude
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Admin.Types.Hyperdata.Frame
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getNodeWith)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude

data FrameCalcUpload = FrameCalcUpload ()
  deriving (Generic)

instance FromForm FrameCalcUpload
instance FromJSON FrameCalcUpload
instance ToJSON FrameCalcUpload
instance ToSchema FrameCalcUpload

type FrameCalcUploadAPI = Summary " FrameCalc upload"
                        :> "add"
                        :> "framecalc"
                        :> "async"
                        :> AsyncJobs JobLog '[JSON] FrameCalcUpload JobLog

frameCalcUploadAPI :: UserId -> NodeId -> GargServer FrameCalcUploadAPI
frameCalcUploadAPI uId nId =
  serveJobsAPI $ 
    JobFunction (\p logs -> frameCalcUploadAsync uId nId p (liftBase . logs))


frameCalcUploadAsync :: FlowCmdM env err m
                     => UserId
                     -> NodeId
                     -> FrameCalcUpload
                     -> (JobLog -> m ())
                     -> m JobLog
frameCalcUploadAsync uId nId _f logStatus = do
  logStatus JobLog { _scst_succeeded = Just 0
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }

  printDebug "[frameCalcUploadAsync] uId" uId
  printDebug "[frameCalcUploadAsync] nId" nId

  node <- getNodeWith nId (Proxy :: Proxy HyperdataFrame)
  let (HyperdataFrame { _hf_base = base
                      , _hf_frame_id = frame_id }) = node ^. node_hyperdata

  let csvUrl = base <> "/" <> frame_id <> ".csv"
  printDebug "[frameCalcUploadAsync] csvUrl" csvUrl

  pure      JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 0
                   , _scst_events    = Just []
                   }
