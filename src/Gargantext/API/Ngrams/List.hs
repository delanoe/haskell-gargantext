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
import Data.Either (Either(..))
import Data.HashMap.Strict (HashMap)
import Data.Map (Map, toList)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text, concat, pack, splitOn)
import Data.Vector (Vector)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams (setListNgrams)
import Gargantext.API.Ngrams.List.Types
import Gargantext.API.Ngrams.Prelude (getNgramsList)
import Gargantext.API.Ngrams.Tools (getTermsWith)
import Gargantext.API.Ngrams.Types
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core.NodeStory
import Gargantext.Core.Text.Terms (ExtractedNgrams(..))
import Gargantext.Core.Text.Terms.WithList (buildPatterns, termsInText)
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Database.Action.Flow (saveDocNgramsWith)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Action.Metrics.NgramsByContext (getOccByNgramsOnlyFast')
import Gargantext.Database.Admin.Types.Hyperdata.Document
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Types (Indexed(..))
import Gargantext.Prelude
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Job.Async
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Data.Vector         as Vec
import qualified Prelude             as Prelude
import qualified Protolude           as P
------------------------------------------------------------------------
type GETAPI = Summary "Get List"
            :> "lists"
              :> Capture "listId" ListId
            :> Get '[JSON, HTML] (Headers '[Header "Content-Disposition" Text] NgramsList)
getApi :: GargServer GETAPI
getApi = get

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToJSON a => MimeRender HTML a where
  mimeRender _ = encode

----------------------
type JSONAPI = Summary "Update List"
          :> "lists"
            :> Capture "listId" ListId
          :> "add"
          :> "form"
          :> "async"
            :> AsyncJobs JobLog '[FormUrlEncoded] WithFile JobLog

jsonApi :: GargServer JSONAPI
jsonApi = postAsync

----------------------
type CSVAPI = Summary "Update List (legacy v3 CSV)"
          :> "lists"
            :> Capture "listId" ListId
          :> "csv"
          :> "add"
          :> "form"
          :> "async"
            :> AsyncJobs JobLog '[FormUrlEncoded] WithTextFile JobLog

csvApi :: GargServer CSVAPI
csvApi = csvPostAsync

------------------------------------------------------------------------
get :: HasNodeStory env err m =>
       ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsList)
get lId = do
  lst <- getNgramsList lId
  let (NodeId id') = lId
  return $ addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                             , pack $ show id'
                             , ".json"
                             ]
                     ) lst

------------------------------------------------------------------------
-- TODO : purge list
-- TODO talk
setList :: FlowCmdM env err m
    => ListId
    -> NgramsList
    -> m Bool
setList l m  = do
  -- TODO check with Version for optim
  printDebug "New list as file" l
  _ <- mapM (\(nt, Versioned _v ns) -> setListNgrams l nt ns) $ toList m
  -- TODO reindex
  pure True

------------------------------------------------------------------------
-- | Re-index documents of a corpus with new ngrams (called orphans here)
reIndexWith :: ( HasNodeStory env err m
               , FlowCmdM     env err m
               )
            => CorpusId
            -> ListId
            -> NgramsType
            -> Set ListType
            -> m ()
reIndexWith cId lId nt lts = do
  -- Getting [NgramsTerm]
  ts <- List.concat
     <$> map (\(k,vs) -> k:vs)
     <$> HashMap.toList
     <$> getTermsWith identity [lId] nt lts
  
  printDebug "ts" ts

  -- Taking the ngrams with 0 occurrences only (orphans)
  occs <- getOccByNgramsOnlyFast' cId lId nt ts

  printDebug "occs" occs

  let orphans = List.concat 
              $ map (\t -> case HashMap.lookup t occs of
                       Nothing -> [t]
                       Just n  -> if n <= 1 then [t] else [ ]
                       ) ts

  printDebug "orphans" orphans

  -- Get all documents of the corpus
  docs <- selectDocNodes cId
  printDebug "docs length" (List.length docs)

  -- Checking Text documents where orphans match
  -- TODO Tests here
  let
    ngramsByDoc = map (HashMap.fromList)
                $ map (map (\(k,v) -> (SimpleNgrams (text2ngrams k), v)))
                $ map (\doc -> List.zip
                                (termsInText (buildPatterns $ map (\k -> (Text.splitOn " " $ unNgramsTerm k, [])) orphans)
                                             $ Text.unlines $ catMaybes
                                               [ doc ^. context_hyperdata . hd_title
                                               , doc ^. context_hyperdata . hd_abstract
                                               ]
                                 )
                                (List.cycle [Map.fromList $ [(nt, Map.singleton (doc ^. context_id) 1 )]])
                        ) docs

  printDebug "ngramsByDoc" ngramsByDoc

  -- Saving the indexation in database
  _ <- mapM (saveDocNgramsWith lId) ngramsByDoc

  pure () -- ngramsByDoc

toIndexedNgrams :: HashMap Text NgramsId -> Text -> Maybe (Indexed Int Ngrams)
toIndexedNgrams m t = Indexed <$> i <*> n
  where
    i = HashMap.lookup t m
    n = Just (text2ngrams t)

------------------------------------------------------------------------
type PostAPI = Summary "Update List"
        :> "add"
        :> "form"
        :> "async"
        :> AsyncJobs JobLog '[FormUrlEncoded] WithFile JobLog

postAsync :: GargServer JSONAPI
postAsync lId =
  serveJobsAPI $
    JobFunction (\f log' ->
      let
        log'' x = do
          printDebug "postAsync ListId" x
          liftBase $ log' x
      in postAsync' lId f log'')

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
  printDebug "New list as file" l
  _ <- setList l m
  -- printDebug "Done" r

  pure JobLog { _scst_succeeded = Just 1
              , _scst_failed    = Just 0
              , _scst_remaining = Just 0
              , _scst_events    = Just []
              }
------------------------------------------------------------------------
type CSVPostAPI = Summary "Update List (legacy v3 CSV)"
        :> "csv"
        :> "add"
        :> "form"
        :> "async"
        :> AsyncJobs JobLog '[FormUrlEncoded] WithFile JobLog

readCsvText :: Text -> [(Text, Text, Text)]
readCsvText t = case eDec of
  Left _ -> []
  Right dec -> Vec.toList dec
  where
    lt = BSL.fromStrict $ P.encodeUtf8 t
    eDec = Csv.decodeWith
             (Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (P.ord '\t') })
             Csv.HasHeader lt :: Either Prelude.String (Vector (Text, Text, Text))

parseCsvData :: [(Text, Text, Text)] -> Map NgramsTerm NgramsRepoElement
parseCsvData lst = Map.fromList $ conv <$> lst
  where
    conv (status, label, forms) =
        (NgramsTerm label, NgramsRepoElement { _nre_size = 1
                                             , _nre_list = case status == "map" of
                                                             True  -> MapTerm
                                                             False -> case status == "main" of
                                                                True  -> CandidateTerm
                                                                False -> StopTerm
                                             , _nre_root = Nothing
                                             , _nre_parent = Nothing
                                             , _nre_children = MSet
                                                             $ Map.fromList
                                                             $ map (\form -> (NgramsTerm form, ()))
                                                             $ filter (/= "")
                                                             $ splitOn "|&|" forms
                                             }
         )

csvPost :: FlowCmdM env err m
        => ListId
        -> Text
        -> m Bool
csvPost l m  = do
  printDebug "[csvPost] l" l
  -- printDebug "[csvPost] m" m
  -- status label forms
  let lst = readCsvText m
  let p = parseCsvData lst
  --printDebug "[csvPost] lst" lst
  printDebug "[csvPost] p" p
  _ <- setListNgrams l NgramsTerms p
  pure True

------------------------------------------------------------------------
csvPostAsync :: GargServer CSVAPI
csvPostAsync lId =
  serveJobsAPI $
    JobFunction $ \f@(WithTextFile ft _ n) log' -> do
      let log'' x = do
            printDebug "[csvPostAsync] filetype" ft
            printDebug "[csvPostAsync] name" n
            liftBase $ log' x
      csvPostAsync' lId f log''


csvPostAsync' :: FlowCmdM env err m
             => ListId
             -> WithTextFile
             -> (JobLog -> m ())
             -> m JobLog
csvPostAsync' l (WithTextFile _ m _) logStatus = do
  logStatus JobLog { _scst_succeeded = Just 0
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }
  _r <- csvPost l m

  pure JobLog { _scst_succeeded = Just 1
              , _scst_failed    = Just 0
              , _scst_remaining = Just 0
              , _scst_events    = Just []
              }
------------------------------------------------------------------------
