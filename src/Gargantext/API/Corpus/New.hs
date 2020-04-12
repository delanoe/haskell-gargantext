{-|
Module      : Gargantext.API.Corpus.New
Description : New corpus API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

New corpus means either:
- new corpus
- new data in existing corpus
-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Corpus.New
      where

import Control.Lens hiding (elements)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Maybe (fromMaybe)
import Data.Either
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Corpus.New.File
import Gargantext.API.Orchestrator.Types
import Gargantext.Core (Lang(..))
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Action.Flow (FlowCmdM, flowCorpus, flowCorpusSearchInDatabase)
import Gargantext.Database.Admin.Types.Node (CorpusId, ToHyperdataDocument(..))
import Gargantext.Core.Types.Individu (UserId, User(..))
import Gargantext.Prelude
import qualified Gargantext.Text.Corpus.Parsers as Parser (FileFormat(..), parseFormat)
import Gargantext.Text.Terms (TermType(..))
import Servant
import Servant.API.Flatten (Flat)
import Servant.Job.Core
import Servant.Job.Types
import Servant.Job.Utils (jsonOptions)
import Servant.Multipart
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Web.FormUrlEncoded          (FromForm)
import qualified Gargantext.Text.Corpus.API as API

data Query = Query { query_query      :: Text
                   , query_corpus_id  :: Int
                   , query_databases  :: [API.ExternalAPIs]
                   }
                   deriving (Eq, Show, Generic)

deriveJSON (unPrefix "query_") 'Query

instance Arbitrary Query where
    arbitrary = elements [ Query q n fs
                         | q <- ["a","b"]
                         , n <- [0..10]
                         , fs <- take 3 $ repeat API.externalAPIs
                         ]

instance ToSchema Query where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "query_")

------------------------------------------------------------------------

type Api = PostApi
        :<|> GetApi

type PostApi = Summary "New Corpus endpoint"
             :> ReqBody '[JSON] Query
             :> Post    '[JSON] CorpusId
type GetApi = Get '[JSON] ApiInfo

-- | TODO manage several apis
-- TODO-ACCESS
-- TODO this is only the POST
api :: (FlowCmdM env err m) => UserId -> Query -> m CorpusId
api uid (Query q _ as) = do
  cId <- case head as of
    Nothing      -> flowCorpusSearchInDatabase (UserDBId uid) EN q
    Just API.All -> flowCorpusSearchInDatabase (UserDBId uid) EN q
    Just a   -> do
      docs <- liftBase $ API.get a q (Just 1000)
      cId' <- flowCorpus (UserDBId uid) (Left q) (Multi EN) [docs]
      pure cId'

  pure cId

------------------------------------------------
data ApiInfo = ApiInfo { api_info :: [API.ExternalAPIs]}
  deriving (Generic)
instance Arbitrary ApiInfo where
  arbitrary = ApiInfo <$> arbitrary

deriveJSON (unPrefix "") 'ApiInfo

instance ToSchema ApiInfo

info :: FlowCmdM env err m => UserId -> m ApiInfo
info _u = pure $ ApiInfo API.externalAPIs

------------------------------------------------------------------------
------------------------------------------------------------------------
data WithQuery = WithQuery
  { _wq_query     :: !Text
  , _wq_databases :: ![ExternalAPIs]
  , _wq_lang      :: !(Maybe Lang)
  }
  deriving Generic

makeLenses ''WithQuery
instance FromJSON WithQuery where
  parseJSON = genericParseJSON $ jsonOptions "_wq_"
instance ToSchema WithQuery where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wq_")

-------------------------------------------------------
data WithForm = WithForm
  { _wf_filetype :: !FileType
  , _wf_data     :: !Text
  , _wf_lang     :: !(Maybe Lang)
  , _wf_name     :: !Text
  } deriving (Eq, Show, Generic)

makeLenses ''WithForm
instance FromForm WithForm
instance FromJSON WithForm where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToSchema WithForm where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")

------------------------------------------------------------------------
type AsyncJobs event ctI input output =
  Flat (AsyncJobsAPI' 'Unsafe 'Safe ctI '[JSON] Maybe event input output)
------------------------------------------------------------------------

type Upload = Summary "Corpus Upload endpoint"
   :> "corpus"
   :> Capture "corpus_id" CorpusId
    :<|> "addWithquery" :> AsyncJobsAPI ScraperStatus                   WithQuery ScraperStatus
    :<|> "addWithfile"  :> AsyncJobs    ScraperStatus '[FormUrlEncoded] WithForm  ScraperStatus

type AddWithQuery = Summary "Add with Query to corpus endpoint"
   :> "corpus"
   :> Capture "corpus_id" CorpusId
   :> "add"
   :> "query"
   :> "async"
   :> AsyncJobsAPI ScraperStatus WithQuery ScraperStatus

type AddWithFile = Summary "Add with MultipartData to corpus endpoint"
   :> "corpus"
   :> Capture "corpus_id" CorpusId
   :> "add" 
   :> "file"
   :> MultipartForm Mem (MultipartData Mem)
   :> QueryParam "fileType"  FileType
   :> "async"
   :> AsyncJobs ScraperStatus '[JSON] () ScraperStatus

type AddWithForm = Summary "Add with FormUrlEncoded to corpus endpoint"
   :> "corpus"
   :> Capture "corpus_id" CorpusId
   :> "add"
   :> "form"
   :> "async"
   :> AsyncJobs ScraperStatus '[FormUrlEncoded] WithForm ScraperStatus

------------------------------------------------------------------------
-- TODO WithQuery also has a corpus id
addToCorpusWithQuery :: FlowCmdM env err m
                       => User
                       -> CorpusId
                       -> WithQuery
                       -> (ScraperStatus -> m ())
                       -> m ScraperStatus
addToCorpusWithQuery u cid (WithQuery q _dbs l) logStatus = do
  -- TODO ...
  logStatus ScraperStatus { _scst_succeeded = Just 10
                          , _scst_failed    = Just 2
                          , _scst_remaining = Just 138
                          , _scst_events    = Just []
                          }
  printDebug "addToCorpusWithQuery" cid
  -- TODO add cid
  -- TODO if cid is folder -> create Corpus
  --      if cid is corpus -> add to corpus
  --      if cid is root   -> create corpus in Private
  cids <- flowCorpusSearchInDatabase u (maybe EN identity l) q
  printDebug "corpus id" cids
  -- TODO ...
  pure      ScraperStatus { _scst_succeeded = Just 137
                          , _scst_failed    = Just 13
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }

addToCorpusWithFile :: FlowCmdM env err m
                    => CorpusId
                    -> MultipartData Mem
                    -> Maybe FileType
                    -> (ScraperStatus -> m ())
                    -> m ScraperStatus
addToCorpusWithFile cid input filetype logStatus = do
  logStatus ScraperStatus { _scst_succeeded = Just 10
                          , _scst_failed    = Just 2
                          , _scst_remaining = Just 138
                          , _scst_events    = Just []
                          }
  printDebug "addToCorpusWithFile" cid
  _h <- postUpload cid filetype input

  pure      ScraperStatus { _scst_succeeded = Just 137
                          , _scst_failed    = Just 13
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }

{- | Model to fork the flow
-- This is not really optimized since it increases the need RAM
-- and freezes the whole system
-- This is mainly for documentation (see a better solution in the function below)
-- Each process has to be tailored
addToCorpusWithForm' :: FlowCmdM env err m
                    => CorpusId
                    -> WithForm
                    -> (ScraperStatus -> m ())
                    -> m ScraperStatus
addToCorpusWithForm' cid (WithForm ft d l) logStatus = do
  newStatus <- liftBase newEmptyMVar
  s  <- addToCorpusWithForm cid (WithForm ft d l) logStatus
  _  <- liftBase $ forkIO $ putMVar newStatus s
  s' <- liftBase $ takeMVar newStatus
  pure s'
-}
addToCorpusWithForm :: FlowCmdM env err m
                    => User
                    -> CorpusId
                    -> WithForm
                    -> (ScraperStatus -> m ())
                    -> m ScraperStatus
addToCorpusWithForm user cid (WithForm ft d l _n) logStatus = do

  let
    parse = case ft of
      CSV_HAL   -> Parser.parseFormat Parser.CsvHal
      CSV       -> Parser.parseFormat Parser.CsvGargV3
      WOS       -> Parser.parseFormat Parser.WOS
      PresseRIS -> Parser.parseFormat Parser.RisPresse

  logStatus ScraperStatus { _scst_succeeded = Just 1
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 1
                          , _scst_events    = Just []
                          }

  printDebug "Parsing corpus: " cid

  -- TODO granularity of the logStatus
  docs <- liftBase $ splitEvery 500
      <$> take 1000000
      <$> parse (cs d)

  printDebug "Parsing corpus finished : " cid
  printDebug "Starting extraction     : " cid

  -- TODO granularity of the logStatus
  _cid' <- flowCorpus user
                     (Right [cid])
                     (Multi $ fromMaybe EN l)
                     (map (map toHyperdataDocument) docs)

  printDebug "Extraction finished   : " cid

  pure      ScraperStatus { _scst_succeeded = Just 2
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }

