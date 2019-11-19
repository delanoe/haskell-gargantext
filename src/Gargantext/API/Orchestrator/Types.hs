{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Orchestrator.Types where

import Gargantext.Prelude
import Control.Lens hiding (elements)
import Data.Aeson
import Data.Text (Text)
import Data.Swagger hiding (URL, url, port)
import GHC.Generics hiding (to)
import Servant.Job.Async
import Servant.Job.Client
import Servant.Job.Types
import Servant.Job.Utils (jsonOptions)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

instance Arbitrary a => Arbitrary (JobStatus 'Safe a) where
  arbitrary = panic "TODO"

instance Arbitrary a => Arbitrary (JobOutput a) where
  arbitrary = JobOutput <$> arbitrary

instance ToSchema URL where
  declareNamedSchema = panic "TODO"

instance ToSchema AnyOutput where
  declareNamedSchema = panic "TODO"

instance ToSchema AnyInput where
  declareNamedSchema = panic "TODO"

instance ToSchema AnyEvent where
  declareNamedSchema = panic "TODO"

instance ToSchema a => ToSchema (JobInput a)

instance ToSchema a => ToSchema (JobOutput a)

-- | Main Types
data ExternalAPIs = All
                  | PubMed

                  | HAL_EN
                  | HAL_FR

                  | IsTex_EN
                  | IsTex_FR

                  | Isidore_EN
                  | Isidore_FR
                  -- | IsidoreAuth
  deriving (Show, Eq, Enum, Bounded, Generic)


-- | Main Instances
instance FromJSON ExternalAPIs
instance ToJSON ExternalAPIs

externalAPIs :: [ExternalAPIs]
externalAPIs = [minBound..maxBound]

instance Arbitrary ExternalAPIs
  where
    arbitrary = elements externalAPIs

instance ToSchema ExternalAPIs

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
data ScraperInput2 = ScraperInput2
  { _scin2_query     :: !Text
  , _scin2_corpus    :: !Int
  , _scin2_databases :: [ExternalAPIs]
  }
  deriving Generic

makeLenses ''ScraperInput2

instance FromJSON ScraperInput2 where
  parseJSON = genericParseJSON $ jsonOptions "_scin2_"

data ScraperEvent = ScraperEvent
  { _scev_message :: !(Maybe Text)
  , _scev_level   :: !(Maybe Text)
  , _scev_date    :: !(Maybe Text)
  }
  deriving Generic

instance Arbitrary ScraperEvent where
  arbitrary = ScraperEvent <$> elements [Nothing, Just "test message"]
                           <*> elements [Nothing, Just "INFO", Just "WARN"]
                           <*> elements [Nothing, Just "2018-04-18"]

instance ToJSON ScraperEvent where
  toJSON = genericToJSON $ jsonOptions "_scev_"

instance FromJSON ScraperEvent where
  parseJSON = genericParseJSON $ jsonOptions "_scev_"

data ScraperStatus = ScraperStatus
  { _scst_succeeded :: !(Maybe Int)
  , _scst_failed    :: !(Maybe Int)
  , _scst_remaining :: !(Maybe Int)
  , _scst_events    :: !(Maybe [ScraperEvent])
  }
  deriving Generic

instance Arbitrary ScraperStatus where
  arbitrary = ScraperStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToJSON ScraperStatus where
  toJSON = genericToJSON $ jsonOptions "_scst_"

instance FromJSON ScraperStatus where
  parseJSON = genericParseJSON $ jsonOptions "_scst_"

instance ToSchema ScraperStatus -- TODO _scst_ prefix

instance ToSchema ScraperInput  -- TODO _scin_ prefix
instance ToSchema ScraperEvent  -- TODO _scev_ prefix

instance ToParamSchema Offset where
  toParamSchema = panic "TODO"

instance ToParamSchema Limit where
  toParamSchema = panic "TODO"

type ScrapersEnv = JobEnv ScraperStatus ScraperStatus

type ScraperAPI = AsyncJobsAPI ScraperStatus ScraperInput ScraperStatus
type ScraperAPI2 = AsyncJobsAPI ScraperStatus ScraperInput2 ScraperStatus
