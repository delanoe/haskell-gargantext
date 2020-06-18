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
import Gargantext.API.Prelude (GargServer, simuLogs)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Prelude (Ord, Eq, (<$>), ($), liftBase, (.))
import Prelude (Enum, Bounded, minBound, maxBound)
import Servant
import Servant.Job.Async (JobFunction(..), serveJobsAPI)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary




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
  simuLogs logStatus 100

------------------------------------------------------------------------
type API = Summary " Share Node with username"
         :> AsyncJobs ScraperStatus '[JSON] UpdateNodeParams ScraperStatus
