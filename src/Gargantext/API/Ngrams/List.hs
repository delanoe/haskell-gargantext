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

import Control.Lens hiding (elements, Indexed)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Map (toList, fromList)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Swagger (ToSchema, declareNamedSchema, genericDeclareNamedSchema)
import Data.Text (Text, concat, pack)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams (getNgramsTableMap, setListNgrams)
import Gargantext.API.Ngrams.Tools (getTermsWith)
import Gargantext.API.Ngrams.Types (RepoCmdM, Versioned(..), NgramsList, NgramsTerm(..))
import Gargantext.API.Node.Corpus.New.File (FileType(..))
import Gargantext.API.Prelude (GargServer, GargNoServer)
import Gargantext.Core.Text.Terms.WithList (buildPatterns, termsInText)
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Action.Flow.Utils (insertDocNgrams)
import Gargantext.Database.Action.Metrics.NgramsByNode (getOccByNgramsOnlyFast')
import Gargantext.Database.Admin.Types.Hyperdata.Document
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Ngrams (insertNgrams)
import Gargantext.Database.Query.Table.Node (getDocumentsWithParentId)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node
import Gargantext.Database.Types (Indexed(..))
import Gargantext.Prelude
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Job.Async
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Map            as Map
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
  let (NodeId id') = lId
  return $ addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                             , pack $ show id'
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
            -> Set ListType
            -> GargNoServer ()
reIndexWith cId lId nt lts = do
  -- Getting [NgramsTerm]
  ts <- List.concat
     <$> map (\(k,vs) -> k:vs)
     <$> HashMap.toList
     <$> getTermsWith identity [lId] nt lts
  
--   printDebug "ts" ts

  -- Taking the ngrams with 0 occurrences only (orphans)
  orphans <-  HashMap.keys
         <$> HashMap.filter (==0)
         <$> getOccByNgramsOnlyFast' cId lId nt ts

  -- Getting the Id of orphan ngrams
  mapTextNgramsId <- insertNgrams (map (text2ngrams . unNgramsTerm) orphans)

  printDebug "orphans" orphans

  -- Get all documents of the corpus
  docs <- getDocumentsWithParentId cId

  -- Checking Text documents where orphans match
  -- TODO Tests here
  let
    ngramsByDoc =  List.concat
               $  map (\doc -> List.zip
                                (termsInText (buildPatterns $ map (\k -> ([unNgramsTerm k], [])) orphans)
                                             $ Text.unlines $ catMaybes
                                               [ doc ^. node_hyperdata . hd_title
                                               , doc ^. node_hyperdata . hd_abstract
                                               ]
                                 )
                                (List.cycle [Map.fromList $ [(nt, Map.singleton (doc ^. node_id) 1 )]])
                        ) docs

  -- Saving the indexation in database
  _ <- insertDocNgrams lId ( HashMap.fromList
                           $ catMaybes
                           $ map (\(t,d) -> (,) <$> toIndexedNgrams mapTextNgramsId t
                                                <*> Just d ) ngramsByDoc
                           )
  pure ()

toIndexedNgrams :: HashMap Text NgramsId -> Text -> Maybe (Indexed Int Ngrams)
toIndexedNgrams m t = Indexed <$> i <*> n
  where
    i = HashMap.lookup t m
    n = Just (text2ngrams t)

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
