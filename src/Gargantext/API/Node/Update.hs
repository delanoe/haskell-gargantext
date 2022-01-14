{-|
Module      : Gargantext.API.Node.Update
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

module Gargantext.API.Node.Update
      where

import Control.Lens (view)
import Data.Aeson
import Data.Maybe (Maybe(..))
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import qualified Gargantext.API.Metrics as Metrics
import Gargantext.API.Ngrams.List (reIndexWith)
import qualified Gargantext.API.Ngrams.Types as NgramsTypes
import Gargantext.API.Prelude (GargServer, simuLogs)
import Gargantext.Core.Methods.Distances (GraphMetric(..))
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Core.Viz.Graph.API (recomputeGraph)
import Gargantext.Database.Action.Flow.Pairing (pairing)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Schema.Node (node_parent_id)
import Gargantext.Database.Schema.Ngrams (NgramsType(NgramsTerms))
import Gargantext.Prelude (Bool(..), Ord, Eq, (<$>), ($), liftBase, (.), printDebug, pure, show, cs, (<>), panic)
import qualified Gargantext.Utils.Aeson as GUA
import Prelude (Enum, Bounded, minBound, maxBound)
import Servant
import Servant.Job.Async (JobFunction(..), serveJobsAPI)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import qualified Data.Set as Set

------------------------------------------------------------------------
type API = Summary " Update node according to NodeType params"
         :> AsyncJobs JobLog '[JSON] UpdateNodeParams JobLog

------------------------------------------------------------------------
data UpdateNodeParams = UpdateNodeParamsList  { methodList  :: !Method      }
                      | UpdateNodeParamsGraph { methodGraph :: !GraphMetric }
                      | UpdateNodeParamsTexts { methodTexts :: !Granularity }
                      | UpdateNodeParamsBoard { methodBoard :: !Charts      }
                      | LinkNodeReq { nodeType :: !NodeType, id :: !NodeId }
    deriving (Generic)

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Granularity = NewNgrams | NewTexts | Both
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Charts = Sources | Authors | Institutes | Ngrams | All
    deriving (Generic, Eq, Ord, Enum, Bounded)

------------------------------------------------------------------------
api :: UserId -> NodeId -> GargServer API
api uId nId =
  serveJobsAPI $
    JobFunction (\p log'' ->
      let
        log' x = do
          printDebug "updateNode" x
          liftBase $ log'' x
      in updateNode uId nId p (liftBase . log')
      )

updateNode :: (HasSettings env, FlowCmdM env err m)
    => UserId
    -> NodeId
    -> UpdateNodeParams
    -> (JobLog -> m ())
    -> m JobLog
updateNode uId nId (UpdateNodeParamsGraph metric) logStatus = do

  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }

  _ <- recomputeGraph uId nId (Just metric) True

  pure  JobLog { _scst_succeeded = Just 2
               , _scst_failed    = Just 0
               , _scst_remaining = Just 0
               , _scst_events    = Just []
               }

updateNode _uId nid1 (LinkNodeReq nt nid2) logStatus = do
  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
  _ <- case nt of
    NodeAnnuaire -> pairing nid2 nid1 Nothing -- defaultList
    NodeCorpus   -> pairing nid1 nid2 Nothing -- defaultList
    _            -> panic $ "[G.API.N.Update.updateNode] NodeType not implemented"
                           <> cs (show nt)

  pure  JobLog { _scst_succeeded = Just 2
               , _scst_failed    = Just 0
               , _scst_remaining = Just 0
               , _scst_events    = Just []
               }

-- | `Advanced` to update graphs
updateNode _uId lId (UpdateNodeParamsList Advanced) logStatus = do
  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 2
                   , _scst_events    = Just []
                   }
  corpusId <- view node_parent_id <$> getNode lId

  logStatus JobLog { _scst_succeeded = Just 2
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }

  _ <- case corpusId of
    Just cId -> do
      _ <- Metrics.updatePie' cId (Just lId) NgramsTypes.Authors Nothing
      _ <- Metrics.updateTree' cId (Just lId) NgramsTypes.Institutes MapTerm
      _ <- Metrics.updatePie' cId (Just lId) NgramsTypes.Sources Nothing
      pure ()
    Nothing  -> pure ()

  pure  JobLog { _scst_succeeded = Just 3
               , _scst_failed    = Just 0
               , _scst_remaining = Just 0
               , _scst_events    = Just []
               }

updateNode _uId lId (UpdateNodeParamsList _mode) logStatus = do
  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 2
                   , _scst_events    = Just []
                   }
  corpusId <- view node_parent_id <$> getNode lId

  logStatus JobLog { _scst_succeeded = Just 2
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }

  _ <- case corpusId of
    Just cId -> reIndexWith cId lId NgramsTerms (Set.singleton MapTerm)
    Nothing  -> pure ()

  pure  JobLog { _scst_succeeded = Just 3
               , _scst_failed    = Just 0
               , _scst_remaining = Just 0
               , _scst_events    = Just []
               }


updateNode _uId _nId _p logStatus = do
  simuLogs logStatus 10

------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  UpdateNodeParams where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })

instance ToJSON    UpdateNodeParams where
  toJSON = genericToJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })
  
instance ToSchema  UpdateNodeParams
instance Arbitrary UpdateNodeParams where
  arbitrary = do
    l <- UpdateNodeParamsList  <$> arbitrary
    g <- UpdateNodeParamsGraph <$> arbitrary
    t <- UpdateNodeParamsTexts <$> arbitrary
    b <- UpdateNodeParamsBoard <$> arbitrary
    elements [l,g,t,b]

instance FromJSON  Method
instance ToJSON    Method
instance ToSchema  Method
instance Arbitrary Method where
  arbitrary = elements [ minBound .. maxBound ]

instance FromJSON  Granularity
instance ToJSON    Granularity
instance ToSchema  Granularity
instance Arbitrary Granularity where
  arbitrary = elements [ minBound .. maxBound ]

instance FromJSON  Charts
instance ToJSON    Charts
instance ToSchema  Charts
instance Arbitrary Charts where
  arbitrary = elements [ minBound .. maxBound ]

------------------------------------------------------------------------
