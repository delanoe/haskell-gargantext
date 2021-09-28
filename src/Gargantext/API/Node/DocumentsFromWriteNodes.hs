{-|
Module      : Gargantext.API.Node.DocumentsFromWriteNodes
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.DocumentsFromWriteNodes
      where

import Data.Aeson
import Data.Swagger
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Prelude (GargServer)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude
import GHC.Generics (Generic)
import Servant
import Servant.Job.Async (JobFunction(..), serveJobsAPI)

------------------------------------------------------------------------
type API = Summary " Documents from Write nodes."
         :> AsyncJobs JobLog '[JSON] Params JobLog
------------------------------------------------------------------------
newtype Params = Params { id :: Int }
  deriving (Generic, Show)

instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Params where
  toJSON = genericToJSON defaultOptions
instance ToSchema Params
------------------------------------------------------------------------
api :: UserId -> NodeId -> GargServer API
api uId nId =
  serveJobsAPI $
    JobFunction (\p log'' ->
      let
        log' x = do
          printDebug "documents from write nodes" x
          liftBase $ log'' x
      in documentsFromWriteNodes uId nId p (liftBase . log')
      )

documentsFromWriteNodes :: (HasSettings env, FlowCmdM env err m)
    => UserId
    -> NodeId
    -> Params
    -> (JobLog -> m ())
    -> m JobLog
documentsFromWriteNodes uId nId p logStatus = do

  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }

  _ <- printDebug "[documentsFromWriteNodes] inside job, uId" uId
  _ <- printDebug "[documentsFromWriteNodes] inside job, nId" nId
  _ <- printDebug "[documentsFromWriteNodes] inside job, p" p

  pure  JobLog { _scst_succeeded = Just 2
               , _scst_failed    = Just 0
               , _scst_remaining = Just 0
               , _scst_events    = Just []
               }
------------------------------------------------------------------------
