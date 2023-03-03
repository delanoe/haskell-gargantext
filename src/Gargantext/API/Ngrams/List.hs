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
import Data.Either (Either(..))
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map, toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text, concat, pack, splitOn)
import Data.Vector (Vector)
import Gargantext.API.Admin.EnvTypes (Env, GargJob(..))
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Ngrams (setListNgrams)
import Gargantext.API.Ngrams.List.Types
import Gargantext.API.Ngrams.Prelude (getNgramsList)
import Gargantext.API.Ngrams.Tools (getTermsWith)
import Gargantext.API.Ngrams.Types
import Gargantext.API.Prelude (GargServer, GargM, GargError)
import Gargantext.API.Types
import Gargantext.Core.NodeStory
import Gargantext.Core.Text.Terms (ExtractedNgrams(..))
import Gargantext.Core.Text.Terms.WithList (buildPatterns, termsInText)
import Gargantext.Core.Types.Main (ListType(..))
import Gargantext.Database.Action.Flow (saveDocNgramsWith)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Hyperdata.Document
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Query.Table.NodeContext (selectDocNodes)
import Gargantext.Database.Schema.Context
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node (_node_parent_id)
import Gargantext.Database.Types (Indexed(..))
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI)
import Servant
-- import Servant.Job.Async
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.Vector         as Vec
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams
import qualified Gargantext.Utils.Servant as GUS
import qualified Prelude
import qualified Protolude           as P
------------------------------------------------------------------------
type GETAPI = Summary "Get List"
            :> "lists"
              :> Capture "listId" ListId
              :> "json"
              :> Get '[JSON, HTML] (Headers '[Header "Content-Disposition" Text] NgramsList)
            :<|> "lists"
              :> Capture "listId" ListId
              :> "csv"
              :> Get '[GUS.CSV] (Headers '[Header "Content-Disposition" Text] NgramsTableMap)
getApi :: GargServer GETAPI
getApi = getJson :<|> getCsv

----------------------
type JSONAPI = Summary "Update List"
          :> "lists"
            :> Capture "listId" ListId
          :> "add"
          :> "form"
          :> "async"
            :> AsyncJobs JobLog '[FormUrlEncoded] WithJsonFile JobLog

jsonApi :: ServerT JSONAPI (GargM Env GargError)
jsonApi = jsonPostAsync

----------------------
type CSVAPI = Summary "Update List (legacy v3 CSV)"
          :> "lists"
            :> Capture "listId" ListId
          :> "csv"
          :> "add"
          :> "form"
          :> "async"
            :> AsyncJobs JobLog '[FormUrlEncoded] WithTextFile JobLog

csvApi :: ServerT CSVAPI (GargM Env GargError)
csvApi = csvPostAsync

------------------------------------------------------------------------
getJson :: HasNodeStory env err m =>
       ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsList)
getJson lId = do
  lst <- getNgramsList lId
  let (NodeId id') = lId
  return $ addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                             , pack $ show id'
                             , ".json"
                             ]
                     ) lst

getCsv :: HasNodeStory env err m =>
       ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsTableMap)
getCsv lId = do
  lst <- getNgramsList lId
  let (NodeId id') = lId
  return $ case Map.lookup TableNgrams.NgramsTerms lst of
    Nothing -> noHeader Map.empty
    Just (Versioned { _v_data }) ->
      addHeader (concat [ "attachment; filename=GarganText_NgramsList-"
                        , pack $ show id'
                        , ".csv"
                        ]
                ) _v_data

------------------------------------------------------------------------
-- TODO : purge list
-- TODO talk
setList :: FlowCmdM env err m
    => ListId
    -> NgramsList
    -> m Bool
setList l m  = do
  -- TODO check with Version for optim
  -- printDebug "New list as file" l
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
  -- printDebug "(cId,lId,nt,lts)" (cId, lId, nt, lts)

  -- Getting [NgramsTerm]
  ts <- List.concat
     <$> map (\(k,vs) -> k:vs)
     <$> HashMap.toList
     <$> getTermsWith identity [lId] nt lts

  -- Get all documents of the corpus
  docs <- selectDocNodes cId

  let
    -- fromListWith (<>)
    ngramsByDoc = map (HashMap.fromListWith (Map.unionWith (Map.unionWith (\(_a,b) (_a',b') -> (1,b+b')))))
                  $ map (map (\((k, cnt), v) -> (SimpleNgrams (text2ngrams k), over (traverse . traverse) (\p -> (p, cnt)) v)))
                  $ map (\doc -> List.zip
                                 (termsInText (buildPatterns $ map (\k -> (Text.splitOn " " $ unNgramsTerm k, [])) ts)
                                              $ Text.unlines $ catMaybes
                                              [ doc ^. context_hyperdata . hd_title
                                              , doc ^. context_hyperdata . hd_abstract
                                              ]
                                 )
                                 (List.cycle [Map.fromList $ [(nt, Map.singleton (doc ^. context_id) 1 )]])
                        ) docs

  -- printDebug "ngramsByDoc: " ngramsByDoc

  -- Saving the indexation in database
  _ <- mapM (saveDocNgramsWith lId) ngramsByDoc

  pure ()

toIndexedNgrams :: HashMap Text NgramsId -> Text -> Maybe (Indexed Int Ngrams)
toIndexedNgrams m t = Indexed <$> i <*> n
  where
    i = HashMap.lookup t m
    n = Just (text2ngrams t)

------------------------------------------------------------------------
jsonPostAsync :: ServerT JSONAPI (GargM Env GargError)
jsonPostAsync lId =
  serveJobsAPI UpdateNgramsListJobJSON $ \f log' ->
      let
        log'' x = do
          -- printDebug "postAsync ListId" x
          liftBase $ log' x
      in postAsync' lId f log''

postAsync' :: FlowCmdM env err m
          => ListId
          -> WithJsonFile
          -> (JobLog -> m ())
          -> m JobLog
postAsync' l (WithJsonFile m _) logStatus = do

  logStatus JobLog { _scst_succeeded = Just 0
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 2
                   , _scst_events    = Just []
                   }
  -- printDebug "New list as file" l
  _ <- setList l m
  -- printDebug "Done" r

  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }


  corpus_node <- getNode l -- (Proxy :: Proxy HyperdataList)
  let corpus_id = fromMaybe (panic "") (_node_parent_id corpus_node)
  _ <- reIndexWith corpus_id l NgramsTerms (Set.fromList [MapTerm, CandidateTerm])

  pure JobLog { _scst_succeeded = Just 2
              , _scst_failed    = Just 0
              , _scst_remaining = Just 0
              , _scst_events    = Just []
              }


------------------------------------------------------------------------

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
                                                             $ filter (\w -> w /= "" && w /= label)
                                                             $ splitOn "|&|" forms
                                             }
         )

csvPost :: FlowCmdM env err m
        => ListId
        -> Text
        -> m Bool
csvPost l m  = do
  -- printDebug "[csvPost] l" l
  -- printDebug "[csvPost] m" m
  -- status label forms
  let lst = readCsvText m
  let p = parseCsvData lst
  --printDebug "[csvPost] lst" lst
  -- printDebug "[csvPost] p" p
  _ <- setListNgrams l NgramsTerms p
  -- printDebug "ReIndexing List" l
  corpus_node <- getNode l -- (Proxy :: Proxy HyperdataList)
  let corpus_id = fromMaybe (panic "") (_node_parent_id corpus_node)
  _ <- reIndexWith corpus_id l NgramsTerms (Set.fromList [MapTerm, CandidateTerm])

  pure True

------------------------------------------------------------------------
csvPostAsync :: ServerT CSVAPI (GargM Env GargError)
csvPostAsync lId =
  serveJobsAPI UpdateNgramsListJobCSV $ \f@(WithTextFile _ft _ _n) log' -> do
      let log'' x = do
            -- printDebug "[csvPostAsync] filetype" ft
            -- printDebug "[csvPostAsync] name" n
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
