{-|
Module      : Gargantext.API.Node.DocumentsFromWriteNodes
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Node.DocumentsFromWriteNodes
      where

-- import Data.Maybe (fromMaybe)
import Conduit
import Control.Lens ((^.))
import Data.Aeson
import Data.Either (Either(..), rights)
import Data.Maybe (fromMaybe)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Admin.EnvTypes (Env, GargJob(..))
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.FrameWrite
import Gargantext.Core.Text.List.Social (FlowSocialListWith)
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (flowDataText, DataText(..))
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Hyperdata.Frame
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getChildrenByType, getClosestParentIdByType', getNodeWith)
import Gargantext.Database.Schema.Node (node_hyperdata, node_name, node_date)
import Gargantext.Prelude
import Gargantext.Utils.Jobs (serveJobsAPI, MonadJobStatus(..))
import Gargantext.Core.Text.Corpus.Parsers.Date (split')
import Servant
import Text.Read (readMaybe)
import qualified Data.List           as List
import qualified Data.Text           as T
-- import qualified Gargantext.Defaults as Defaults

------------------------------------------------------------------------
type API = Summary " Documents from Write nodes."
         :> AsyncJobs JobLog '[JSON] Params JobLog
------------------------------------------------------------------------
data Params = Params
  { id         :: Int
  , paragraphs :: Text
  , lang       :: Lang
  , selection  :: FlowSocialListWith
  }
  deriving (Generic, Show)
instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Params where
  toJSON = genericToJSON defaultOptions
instance ToSchema Params
------------------------------------------------------------------------
api :: UserId -> NodeId -> ServerT API (GargM Env GargError)
api uId nId =
  serveJobsAPI DocumentFromWriteNodeJob $ \jHandle p ->
    documentsFromWriteNodes uId nId p jHandle

documentsFromWriteNodes :: (HasSettings env, FlowCmdM env err m, MonadJobStatus m)
    => UserId
    -> NodeId
    -> Params
    -> JobHandle m
    -> m ()
documentsFromWriteNodes uId nId Params { selection, lang, paragraphs } jobHandle = do
  markStarted 2 jobHandle
  markProgress 1 jobHandle

  mcId <- getClosestParentIdByType' nId NodeCorpus
  cId <- case mcId of
    Just cId -> pure cId
    Nothing -> do
      let msg = T.pack $ "[G.A.N.DFWN] Node has no corpus parent: " <> show nId
      markFailed (Just msg) jobHandle
      panic msg

  frameWriteIds <- getChildrenByType nId NodeFrameWrite

  -- https://write.frame.gargantext.org/<frame_id>/download
  frameWrites <- mapM (\id -> getNodeWith id (Proxy :: Proxy HyperdataFrame)) frameWriteIds

  frameWritesWithContents <- liftBase $
    mapM (\node -> do
             contents <- getHyperdataFrameContents (node ^. node_hyperdata)
             pure (node, contents)
         ) frameWrites

  let paragraphs' = fromMaybe (7 :: Int) $ (readMaybe $ T.unpack paragraphs)
  let parsedE = (\(node, contents)
                  -> hyperdataDocumentFromFrameWrite lang paragraphs' (node, contents)) <$> frameWritesWithContents
  let parsed = List.concat $ rights parsedE
  -- printDebug "DocumentsFromWriteNodes: uId" uId
  _ <- flowDataText (RootId (NodeId uId))
                    (DataNew (Just $ fromIntegral $ length parsed, yieldMany parsed))
                    (Multi lang)
                    cId
                    (Just selection)
                    jobHandle

  markProgress 1 jobHandle

------------------------------------------------------------------------
hyperdataDocumentFromFrameWrite :: Lang -> Int -> (Node HyperdataFrame, T.Text) -> Either T.Text [HyperdataDocument]
hyperdataDocumentFromFrameWrite lang paragraphSize (node, contents) =
  case parseLines contents of
    Left _ -> Left "Error parsing node"
    Right (Parsed { authors, contents = ctxts}) ->
      let HyperdataFrame { _hf_base, _hf_frame_id } = node ^. node_hyperdata
          authorJoinSingle (Author { firstName, lastName }) = T.concat [ lastName, ", ", firstName ]
          authors' = T.concat $ authorJoinSingle <$> authors

--{-
          (year',month',day') = split' (node^. node_date)
          date' = Just $ T.concat [ T.pack $ show year', "-"
                                  , T.pack $ show month', "-"
                                  , T.pack $ show day'
                                  ]
--}

{-
          date' = (\(Date { year, month, day }) -> T.concat [ T.pack $ show year', "-"
                                                            , T.pack $ show month', "-"
                                                            , T.pack $ show day' ]) <$> date
          year' = fromIntegral $ maybe Defaults.year (\(Date { year }) -> year) date
          month' = maybe Defaults.month (\(Date { month }) -> fromIntegral month) date
          day' = maybe Defaults.day (\(Date { day }) -> fromIntegral day) date
--}
          in
      Right (List.map (\(t, ctxt) ->  HyperdataDocument { _hd_bdd = Just "FrameWrite"
                              , _hd_doi = Nothing
                              , _hd_url = Nothing
                              , _hd_uniqId = Nothing
                              , _hd_uniqIdBdd = Nothing
                              , _hd_page = Nothing
                              , _hd_title = Just t
                              , _hd_authors = Just authors'
                              , _hd_institutes = Nothing
                              , _hd_source = Just $ node ^. node_name
                              , _hd_abstract = Just ctxt
                              , _hd_publication_date = date'
                              , _hd_publication_year = Just year'
                              , _hd_publication_month = Just month'
                              , _hd_publication_day = Just day'
                              , _hd_publication_hour = Nothing
                              , _hd_publication_minute = Nothing
                              , _hd_publication_second = Nothing
                              , _hd_language_iso2 = Just $ T.pack $ show lang }
                      ) (text2titleParagraphs paragraphSize ctxts)
                  )
