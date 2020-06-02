{-|
Module      : Gargantext.API.Node.Corpus.New
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

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.Corpus.New
      where

import Control.Lens hiding (elements, Empty)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Either
import Data.Maybe (fromMaybe)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types (ScraperStatus(..))
import qualified Gargantext.API.Admin.Orchestrator.Types as T
import Gargantext.API.Node.Corpus.New.File
import Gargantext.Core (Lang(..){-, allLangs-})
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Action.Flow (FlowCmdM, flowCorpus, getDataText, flowDataText, TermType(..), DataOrigin(..){-, allDataOrigins-})
import Gargantext.Database.Admin.Types.Node (CorpusId, ToHyperdataDocument(..), UserId)
import Gargantext.Prelude
import Servant
import Servant.Job.Core
import Servant.Job.Types
import Servant.Job.Utils (jsonOptions)
-- import Servant.Multipart
-- import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Web.FormUrlEncoded          (FromForm)
import qualified Gargantext.Text.Corpus.API as API
import qualified Gargantext.Text.Corpus.Parsers as Parser (FileFormat(..), parseFormat)

------------------------------------------------------------------------
{-
data Query = Query { query_query      :: Text
                   , query_node_id    :: Int
                   , query_lang       :: Lang
                   , query_databases  :: [DataOrigin]
                   }
                   deriving (Eq, Generic)

deriveJSON (unPrefix "query_") 'Query

instance Arbitrary Query where
    arbitrary = elements [ Query q n la fs
                         | q <- ["honeybee* AND collapse"
                                ,"covid 19"
                                ]
                         , n <- [0..10]
                         , la <- allLangs
                         , fs <- take 3 $ repeat allDataOrigins
                         ]

instance ToSchema Query where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "query_")
-}

------------------------------------------------------------------------

{-
type Api = PostApi
        :<|> GetApi

type PostApi = Summary "New Corpus endpoint"
             :> ReqBody '[JSON] Query
             :> Post    '[JSON] CorpusId
type GetApi = Get '[JSON] ApiInfo
-}

-- | TODO manage several apis
-- TODO-ACCESS
-- TODO this is only the POST
{-
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
-}

------------------------------------------------
-- TODO use this route for Client implementation
data ApiInfo = ApiInfo { api_info :: [API.ExternalAPIs]}
  deriving (Generic)
instance Arbitrary ApiInfo where
  arbitrary = ApiInfo <$> arbitrary

deriveJSON (unPrefix "") 'ApiInfo

instance ToSchema ApiInfo

info :: FlowCmdM env err m => UserId -> m ApiInfo
info _u = pure $ ApiInfo API.externalAPIs

------------------------------------------------------------------------

data Database = Empty
              | PubMed
              | HAL
              | IsTex
              | Isidore
  deriving (Eq, Show, Generic)

deriveJSON (unPrefix "") ''Database
instance ToSchema Database

database2origin :: Database -> DataOrigin
database2origin Empty   = InternalOrigin T.IsTex
database2origin PubMed  = ExternalOrigin T.PubMed
database2origin HAL     = ExternalOrigin T.HAL
database2origin IsTex   = ExternalOrigin T.IsTex
database2origin Isidore = ExternalOrigin T.Isidore

------------------------------------------------------------------------
data WithQuery = WithQuery
  { _wq_query     :: !Text
  , _wq_databases :: !Database
  , _wq_lang      :: !Lang
  , _wq_node_id   :: !Int
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
  AsyncJobsAPI' 'Unsafe 'Safe ctI '[JSON] Maybe event input output
------------------------------------------------------------------------

type AddWithQuery = Summary "Add with Query to corpus endpoint"
   :> "corpus"
     :> Capture "corpus_id" CorpusId
   :> "query"
     :> AsyncJobs ScraperStatus '[JSON] WithQuery ScraperStatus

{-
type AddWithFile = Summary "Add with MultipartData to corpus endpoint"
   :> "corpus"
     :> Capture "corpus_id" CorpusId
   :> "add"
   :> "file"
     :> MultipartForm Mem (MultipartData Mem)
     :> QueryParam "fileType"  FileType
   :> "async"
     :> AsyncJobs ScraperStatus '[JSON] () ScraperStatus
-}

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
addToCorpusWithQuery u cid (WithQuery q dbs l _nid) logStatus = do
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
  txts <- mapM (\db  -> getDataText db     (Multi l) q (Just 10000)) [database2origin dbs]
  cids <- mapM (\txt -> flowDataText u txt (Multi l) cid) txts
  printDebug "corpus id" cids
  -- TODO ...
  pure      ScraperStatus { _scst_succeeded = Just 137
                          , _scst_failed    = Just 13
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }

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

{-
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
-}


