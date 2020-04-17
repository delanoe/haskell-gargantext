{-|
Module      : Gargantext.API.Ngrams.List
Description : Get Ngrams (lists)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Ngrams.List
  where

import Control.Lens hiding (elements)
import Data.Aeson
import Data.List (zip)
import Data.Map (Map, toList, fromList)
import Data.Swagger (ToSchema, declareNamedSchema, genericDeclareNamedSchema)
import Data.Text (Text, concat, pack)
import GHC.Generics (Generic)
import Gargantext.API.Corpus.New
import Gargantext.API.Corpus.New.File (FileType(..))
import Gargantext.API.Ngrams
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Admin.Types (GargServer)
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.Action.Flow (FlowCmdM)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Schema.Ngrams (NgramsType(..), ngramsTypes)
import Gargantext.Prelude
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Job.Async
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm)

------------------------------------------------------------------------
type NgramsList = (Map NgramsType (Versioned NgramsTableMap))
------------------------------------------------------------------------
type API =  Get '[JSON, HTML] (Headers '[Header "Content-Disposition" Text] NgramsList)
       -- :<|> ReqBody '[JSON] NgramsList :> Post '[JSON] Bool
       :<|> PostAPI

api :: ListId -> GargServer API
api l = get l :<|> postAsync l

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToJSON a => MimeRender HTML a where
  mimeRender _ = encode

------------------------------------------------------------------------

get :: RepoCmdM env err m
        => ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsList)
get lId = do
  lst <- get' lId
  let (NodeId id) = lId
  return $ addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                             , pack $ show id
                             , ".json"
                             ]
                     ) lst

get' :: RepoCmdM env err m
    => ListId -> m NgramsList
get' lId = fromList
       <$> zip ngramsTypes
       <$> mapM (getNgramsTableMap lId) ngramsTypes

------------------------------------------------------------------------

-- TODO : purge list
post :: FlowCmdM env err m
    => ListId
    -> NgramsList
    -> m Bool
post l m  = do
  -- TODO check with Version for optim
  _ <- mapM (\(nt, Versioned _v ns) -> setListNgrams l nt ns) $ toList m
  -- TODO reindex
  pure True

------------------------------------------------------------------------
------------------------------------------------------------------------

type PostAPI = Summary "Update List"
        :> "add"
        :> "form"
        :> "async"
        :> AsyncJobs ScraperStatus '[FormUrlEncoded] WithFile ScraperStatus

postAsync :: ListId -> GargServer PostAPI
postAsync lId =
  serveJobsAPI $
    JobFunction (\f  log' -> postAsync' lId f (liftBase . log'))

postAsync' :: FlowCmdM env err m
          => ListId
          -> WithFile
          -> (ScraperStatus -> m ())
          -> m ScraperStatus
postAsync' l (WithFile _ m _) logStatus = do

  logStatus ScraperStatus { _scst_succeeded = Just 0
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 1
                          , _scst_events    = Just []
                          }
  _r <- post l m

  pure ScraperStatus { _scst_succeeded = Just 1
                     , _scst_failed    = Just 0
                     , _scst_remaining = Just 0
                     , _scst_events    = Just []
                     }

data WithFile = WithFile
  { _wf_filetype :: !FileType
  , _wf_data     :: !NgramsList
  , _wf_name     :: !Text
  } deriving (Eq, Show, Generic)

makeLenses ''WithFile
instance FromForm WithFile
instance FromJSON WithFile where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToSchema WithFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")

