{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Admin.Orchestrator.Types
  where

import Control.Lens hiding (elements)
import Control.Monad.Reader (MonadReader)
import Data.Aeson
import Data.Morpheus.Types
  ( GQLType
  , typeOptions )
import Data.Proxy
import Data.Swagger hiding (URL, url, port)
import Data.Text (Text)
import GHC.Generics hiding (to)
import Servant
import Servant.Job.Async
import Servant.Job.Types
import Servant.Job.Utils (jsonOptions)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

import qualified Gargantext.API.GraphQL.Utils as GQLU
import Gargantext.Core.Types (TODO(..))
import Gargantext.Database.Prelude (HasConfig(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (gc_pubmed_api_key)

------------------------------------------------------------------------
instance Arbitrary a => Arbitrary (JobStatus 'Safe a) where
  arbitrary = panic "TODO"

instance Arbitrary a => Arbitrary (JobOutput a) where
  arbitrary = JobOutput <$> arbitrary

-- | Main Types
-- TODO IsidoreAuth
data ExternalAPIs = All
                  | PubMed { mAPIKey :: Maybe Text }
                  | Arxiv
                  | HAL
                  | IsTex
                  | Isidore
  deriving (Show, Eq, Generic)


-- | Main Instances
instance FromJSON ExternalAPIs
instance ToJSON ExternalAPIs

externalAPIs :: ( MonadReader env m
                , HasConfig env) => m [ExternalAPIs]
externalAPIs = do
  pubmed_api_key <- view $ hasConfig . gc_pubmed_api_key

  pure [ All
       , PubMed { mAPIKey = Just pubmed_api_key }
       , Arxiv
       , HAL
       , IsTex
       , Isidore ]

instance Arbitrary ExternalAPIs
  where
    arbitrary = elements [ All
                         , PubMed { mAPIKey = Nothing }
                         , Arxiv
                         , HAL
                         , IsTex
                         , Isidore ]

instance ToSchema ExternalAPIs where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema URL where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

data ScraperInput = ScraperInput
  { _scin_spider       :: !Text
  , _scin_query        :: !(Maybe Text)
  , _scin_user         :: !Text
  , _scin_corpus       :: !Int
  , _scin_report_every :: !(Maybe Int)
  , _scin_limit        :: !(Maybe Int)
  , _scin_local_file   :: !(Maybe Text)
  , _scin_count_only   :: !(Maybe Bool)
  }
  deriving Generic

makeLenses ''ScraperInput

instance FromJSON ScraperInput where
  parseJSON = genericParseJSON $ jsonOptions "_scin_"

-- Proposal to replace the Corpus.API.Query type which seems to generically named.

data ScraperEvent = ScraperEvent
  { _scev_message :: !(Maybe Text)
  , _scev_level   :: !(Maybe Text)
  , _scev_date    :: !(Maybe Text)
  }
  deriving (Show, Generic, Eq)

instance Arbitrary ScraperEvent where
  arbitrary = ScraperEvent <$> elements [Nothing, Just "test message"]
                           <*> elements [Nothing, Just "INFO", Just "WARN"]
                           <*> elements [Nothing, Just "2018-04-18"]
instance ToJSON ScraperEvent where
  toJSON = genericToJSON $ jsonOptions "_scev_"
instance FromJSON ScraperEvent where
  parseJSON = genericParseJSON $ jsonOptions "_scev_"
instance ToSchema ScraperEvent  -- TODO _scev_ prefix
instance GQLType ScraperEvent where
  typeOptions _ = GQLU.unPrefix "_scev_"


data JobLog = JobLog
  { _scst_succeeded :: !(Maybe Int)
  , _scst_failed    :: !(Maybe Int)
  , _scst_remaining :: !(Maybe Int)
  , _scst_events    :: !(Maybe [ScraperEvent])
  }
  deriving (Show, Generic, Eq)

makeLenses ''JobLog

noJobLog :: JobLog
noJobLog = JobLog Nothing Nothing Nothing Nothing

instance Arbitrary JobLog where
  arbitrary = JobLog
           <$> arbitrary
           <*> arbitrary
           <*> arbitrary
           <*> arbitrary
instance ToJSON JobLog where
  toJSON = genericToJSON $ jsonOptions "_scst_"
instance FromJSON JobLog where
  parseJSON = genericParseJSON $ jsonOptions "_scst_"
instance ToSchema JobLog -- TODO _scst_ prefix
instance GQLType JobLog where
  typeOptions _ = GQLU.unPrefix "_scst_"

instance ToSchema ScraperInput  -- TODO _scin_ prefix

instance ToParamSchema Offset -- where
  -- toParamSchema = panic "TODO"

instance ToParamSchema Limit -- where
  -- toParamSchema = panic "TODO"

type ScrapersEnv = JobEnv JobLog JobLog

type ScraperAPI  = AsyncJobsAPI JobLog ScraperInput  JobLog

------------------------------------------------------------------------
type AsyncJobs event ctI input output =
  AsyncJobsAPI' 'Unsafe 'Safe ctI '[JSON] Maybe event input output
