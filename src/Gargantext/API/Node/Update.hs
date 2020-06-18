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

import Data.Aeson
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types (ScraperStatus(..))
import Gargantext.API.Node.Corpus.New (AsyncJobs)
import Gargantext.API.Prelude (GargServer{-, simuLogs-})
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude (Ord, Eq, (<$>), ($), liftBase, (.), Int, (-), pure, (*), (^), printDebug)
import Prelude (Enum, Bounded, minBound, maxBound)
import Servant
import Servant.Job.Async (JobFunction(..), serveJobsAPI)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

import Data.Maybe (Maybe(..))
import Control.Concurrent (threadDelay)



------------------------------------------------------------------------
data UpdateNodeParams = UpdateNodeParamsList  { methodList  :: Method      }
                      | UpdateNodeParamsGraph { methodGraph :: GraphMetric }
                      | UpdateNodeParamsTexts { methodTexts :: Granularity }
                      | UpdateNodeParamsBoard { methodBoard :: Charts      }
    deriving (Generic)

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data GraphMetric = Order1 | Order2
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Granularity = NewNgrams | NewTexts | Both
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Charts = Sources | Authors | Institutes | Ngrams | All
    deriving (Generic, Eq, Ord, Enum, Bounded)

------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  UpdateNodeParams
instance ToJSON    UpdateNodeParams
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

instance FromJSON  GraphMetric
instance ToJSON    GraphMetric
instance ToSchema  GraphMetric
instance Arbitrary GraphMetric where
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
api :: UserId -> NodeId -> GargServer API
api uId nId =
  serveJobsAPI $ 
    JobFunction (\p logs -> updateNode uId nId p (liftBase . logs))


updateNode :: FlowCmdM env err m
    => UserId
    -> NodeId
    -> UpdateNodeParams
    -> (ScraperStatus -> m ())
    -> m ScraperStatus
updateNode _uId _nId _ logStatus = do

-- Why this does not work ?
--  simuLogs logStatus 100

  logStatus $ ScraperStatus { _scst_succeeded = Just 1
                             , _scst_failed    = Just 0
                             , _scst_remaining = Just 10
                             , _scst_events    = Just []
                             }

  let
    m = (10 :: Int) ^ (6 :: Int)
    status t n = do
      _ <- liftBase $ threadDelay ( m * 100)
      let s = ScraperStatus { _scst_succeeded = Just n
                             , _scst_failed    = Just 0
                             , _scst_remaining = Just (t - n)
                             , _scst_events    = Just []
                             }
      printDebug "status " s
      pure s

  s1 <- status 10 2
  logStatus s1

  s2 <- status 10 5
  logStatus s2

  s3 <- status 10 7
  logStatus s3

  status 10 10






------------------------------------------------------------------------
type API = Summary " Share Node with username"
         :> AsyncJobs ScraperStatus '[JSON] UpdateNodeParams ScraperStatus
