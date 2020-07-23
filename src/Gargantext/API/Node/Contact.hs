{-|
Module      : Gargantext.API.Node.Contact
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.Contact
      where

import Data.Aeson
import Data.Either (Either(Right))
import Data.Maybe (Maybe(..))
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..))
import Gargantext.API.Admin.Settings (HasSettings)
import Gargantext.API.Node.Corpus.New (AsyncJobs)
import Gargantext.API.Prelude (GargServer, simuLogs)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (flow)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataAnnuaire(..))
import Gargantext.Database.Admin.Types.Hyperdata.Contact (hyperdataContact)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude (($), liftBase, (.), printDebug, pure)
import Gargantext.Text.Terms (TermType(..))
import Servant
import Servant.Job.Async (JobFunction(..), serveJobsAPI)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

------------------------------------------------------------------------
type API = Summary " Add Contact to Annuaire"
         :> AsyncJobs JobLog '[JSON] AddContactParams JobLog

------------------------------------------------------------------------
data AddContactParams = AddContactParams         { firstname :: !Text, lastname :: !Text }
                      | AddContactParamsAdvanced { firstname :: !Text
                                                 , lastname  :: !Text
                                                 -- TODO add others fields
                                                 }
    deriving (Generic)

----------------------------------------------------------------------
api :: User -> NodeId -> GargServer API
api u nId =
  serveJobsAPI $
    JobFunction (\p log ->
      let
        log' x = do
          printDebug "addContact" x
          liftBase $ log x
      in addContact u nId p (liftBase . log')
      )

addContact :: (HasSettings env, FlowCmdM env err m)
    => User
    -> NodeId
    -> AddContactParams
    -> (JobLog -> m ())
    -> m JobLog
addContact u nId (AddContactParams fn ln) logStatus = do

  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
  _ <- flow (Nothing :: Maybe HyperdataAnnuaire) u (Right [nId]) (Multi EN) [[hyperdataContact fn ln]]

  pure  JobLog { _scst_succeeded = Just 2
               , _scst_failed    = Just 0
               , _scst_remaining = Just 0
               , _scst_events    = Just []
               }


addContact _uId _nId _p logStatus = do
  simuLogs logStatus 10

------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  AddContactParams where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON    AddContactParams where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
  
instance ToSchema  AddContactParams
instance Arbitrary AddContactParams where
  arbitrary = elements [AddContactParams "Pierre" "Dupont"]

------------------------------------------------------------------------
