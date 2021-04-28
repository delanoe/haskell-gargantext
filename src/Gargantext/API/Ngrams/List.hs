{-|
Module      : Gargantext.API.Ngrams.List
Description : Get Ngrams (lists)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Ngrams.List
  where

import Data.Maybe (catMaybes)
import Control.Lens hiding (elements)
import Data.Aeson
import Data.Map (toList, fromList)
import Data.Swagger (ToSchema, declareNamedSchema, genericDeclareNamedSchema)
import Data.Text (Text, concat, pack)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams (getNgramsTableMap, setListNgrams, NgramsTerm)
import Gargantext.API.Ngrams.Types (RepoCmdM, Versioned(..), NgramsList, NgramsTerm(..))
import Gargantext.API.Node.Corpus.New.File (FileType(..))
import Gargantext.API.Prelude (GargServer, GargNoServer)
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Action.Metrics.NgramsByNode (getOccByNgramsOnlyFast')
import Gargantext.Database.Schema.Node
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Admin.Types.Hyperdata.Document
import Gargantext.Database.Schema.Ngrams (ngramsTypes, NgramsType(..))
import Gargantext.Database.Query.Table.Node (getDocumentsWithParentId)
import Gargantext.Prelude
import Gargantext.Core.Text.Terms.WithList (buildPatterns, termsInText)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Job.Async
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text

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
get :: RepoCmdM env err m =>
       ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsList)
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
-- TODO talk
post :: FlowCmdM env err m
    => ListId
    -> NgramsList
    -> m Bool
post l m  = do
  -- TODO check with Version for optim
  _ <- mapM (\(nt, Versioned _v ns) -> setListNgrams l nt ns) $ toList m
  -- TODO reindex
  pure True


-----------------------------------------------------------------------------
-- | Re-index documents of a corpus with new ngrams (called orphans here)
reIndexWith :: CorpusId
            -> ListId
            -> NgramsType
            -> [NgramsTerm]
            -> GargNoServer ()
reIndexWith cId lId nt ts = do
  docs <- getDocumentsWithParentId cId

  -- Taking the ngrams with 0 occurrences only (orphans)
  orphans <-  map (\k -> ([unNgramsTerm k], []))
         <$> HashMap.keys
         <$> HashMap.filter (==0)
         <$> getOccByNgramsOnlyFast' cId lId nt ts

  -- Checking Text documents where orphans match
  let
    docMatched =
        map (\doc -> ( doc ^. node_id
                   , termsInText (buildPatterns orphans)
                                 ( Text.unlines
                                 $ catMaybes
                                 [ doc ^. node_hyperdata . hd_title
                                 , doc ^. node_hyperdata . hd_abstract
                                 ]
                                 )
                  )
         ) docs

  -- Saving the indexation in database

  pure ()

------------------------------------------------------------------------
------------------------------------------------------------------------
type PostAPI = Summary "Update List"
        :> "add"
        :> "form"
        :> "async"
        :> AsyncJobs JobLog '[FormUrlEncoded] WithFile JobLog

postAsync :: ListId -> GargServer PostAPI
postAsync lId =
  serveJobsAPI $
    JobFunction (\f  log' -> postAsync' lId f (liftBase . log'))

postAsync' :: FlowCmdM env err m
          => ListId
          -> WithFile
          -> (JobLog -> m ())
          -> m JobLog
postAsync' l (WithFile _ m _) logStatus = do

  logStatus JobLog { _scst_succeeded = Just 0
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
  _r <- post l m

  pure JobLog { _scst_succeeded = Just 1
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
instance ToJSON WithFile where
  toJSON = genericToJSON $ jsonOptions "_wf_"
instance ToSchema WithFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")
