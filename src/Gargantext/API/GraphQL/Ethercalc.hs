{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.Ethercalc where

import Control.Lens ((^.))
import Data.Either (Either(..))
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , ResolverM
  , QUERY
  , lift
  )
import Data.Text (Text)
import qualified Data.Text as T
import Gargantext.API.Job (jobLogInit)
import qualified Gargantext.API.Node.Corpus.New as New
import Gargantext.API.Prelude (GargM, GargServerT, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Core.Types (NodeId(..))
import Gargantext.Database.Admin.Types.Node (NodeId(..), NodeType)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataFrame, getHyperdataFrameCSV)
import qualified Gargantext.Database.Admin.Types.Node as NN
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType, getNode, getNodeWith)
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import qualified Gargantext.Database.Schema.Node as N
import Gargantext.Prelude
import GHC.Generics (Generic)
import qualified Prelude as Prelude
import Servant (Proxy(..))
import Servant.Job.Async (serveJobsAPI, JobFunction(..))
import Text.Read (readEither)

data EthercalcCSVDownload = EthercalcCSVDownload
  { corpusId :: Int
  , nodeId   :: Int
  } deriving (Show, Generic, GQLType)

data EthercalcCSVDownloadArgs
  = EthercalcCSVDownloadArgs
    { corpusId :: Int
    , nodeId   :: Int
    } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

-- | Function to resolve user from a query.
ethercalcCSVDownload
  :: (HasConnectionPool env, HasConfig env, HasMail env)
  => EthercalcCSVDownloadArgs -> ResolverM e (GargM env GargError) Int
ethercalcCSVDownload (EthercalcCSVDownloadArgs { corpusId, nodeId }) = do
  ret <- lift $ do
    frameCalc <- getNodeWith (NodeId nodeId) (Proxy :: Proxy HyperdataFrame)
    printDebug "[ethercalcCSVDownload] frameCalc" frameCalc
    csv <- liftBase $ getHyperdataFrameCSV (frameCalc ^. N.node_hyperdata)
    printDebug "[ethercalcCSVDownload] csv" csv
    serveJobsAPI $
      JobFunction (\i log' ->
        let
          log'' x = do
            printDebug "[ethercalcCSVDownload] " x
            liftBase $ log' x
        in New.addToCorpusWithForm user corpusId i log'' (jobLogInit 3))
  lift $ printDebug "[ethercalcCSVDownload] ret" ret
  pure 0

