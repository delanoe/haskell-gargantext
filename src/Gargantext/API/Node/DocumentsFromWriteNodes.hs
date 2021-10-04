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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.DocumentsFromWriteNodes
      where

import Control.Lens ((^.))
import Data.Aeson
import Data.Either (Either(..), rights)
import Data.Swagger
import qualified Data.Text as T
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Prelude (GargServer)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.FrameWrite
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (flowDataText, DataText(..))
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Hyperdata.Frame
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getChildrenByType, getClosestParentIdByType', getNodeWith)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import GHC.Generics (Generic)
import Servant
import Servant.Job.Async (JobFunction(..), serveJobsAPI)

------------------------------------------------------------------------
type API = Summary " Documents from Write nodes."
         :> AsyncJobs JobLog '[JSON] Params JobLog
------------------------------------------------------------------------
newtype Params = Params { id :: Int }
  deriving (Generic, Show)

instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Params where
  toJSON = genericToJSON defaultOptions
instance ToSchema Params
------------------------------------------------------------------------
api :: UserId -> NodeId -> GargServer API
api uId nId =
  serveJobsAPI $
    JobFunction (\p log'' ->
      let
        log' x = do
          liftBase $ log'' x
      in documentsFromWriteNodes uId nId p (liftBase . log')
      )

documentsFromWriteNodes :: (HasSettings env, FlowCmdM env err m)
    => UserId
    -> NodeId
    -> Params
    -> (JobLog -> m ())
    -> m JobLog
documentsFromWriteNodes uId nId _p logStatus = do

  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }

  mcId <- getClosestParentIdByType' nId NodeCorpus
  let cId = maybe (panic "[G.A.N.DFWN] Node has no parent") identity mcId

  frameWriteIds <- getChildrenByType nId NodeFrameWrite

  -- https://write.frame.gargantext.org/<frame_id>/download
  frameWrites <- mapM (\id -> getNodeWith id (Proxy :: Proxy HyperdataFrame)) frameWriteIds

  frameWritesWithContents <- liftBase $
    mapM (\node -> do
             contents <- getHyperdataFrameContents (node ^. node_hyperdata)
             pure (node, contents)
         ) frameWrites

  let parsedE = (\(node, contents) -> hyperdataDocumentFromFrameWrite (node ^. node_hyperdata, contents)) <$> frameWritesWithContents
  let parsed = rights parsedE

  _ <- flowDataText (RootId (NodeId uId)) (DataNew [parsed]) (Multi EN) cId Nothing

  pure  JobLog { _scst_succeeded = Just 2
               , _scst_failed    = Just 0
               , _scst_remaining = Just 0
               , _scst_events    = Just []
               }
------------------------------------------------------------------------
hyperdataDocumentFromFrameWrite :: (HyperdataFrame, T.Text) -> Either T.Text HyperdataDocument
hyperdataDocumentFromFrameWrite (HyperdataFrame { _hf_base, _hf_frame_id }, contents) =
  case parseLines contents of
    Left _ -> Left "Error parsing node"
    Right (Parsed { authors, contents = c, date, source, title = t }) ->
      let authorJoinSingle (Author { firstName, lastName }) = T.concat [ lastName, ", ", firstName ] in
      let authors' = T.concat $ authorJoinSingle <$> authors in
      Right HyperdataDocument { _hd_bdd = Just "FrameWrite"
                              , _hd_doi = Nothing
                              , _hd_url = Nothing
                              , _hd_uniqId = Nothing
                              , _hd_uniqIdBdd = Nothing
                              , _hd_page = Nothing
                              , _hd_title = Just t
                              , _hd_authors = Just authors'
                              , _hd_institutes = Nothing
                              , _hd_source = source
                              , _hd_abstract = Just c
                              , _hd_publication_date = date
                              , _hd_publication_year = Just 2021  -- TODO
                              , _hd_publication_month = Just 10  -- TODO
                              , _hd_publication_day = Just 4  -- TODO
                              , _hd_publication_hour = Nothing
                              , _hd_publication_minute = Nothing
                              , _hd_publication_second = Nothing
                              , _hd_language_iso2 = Just $ T.pack $ show EN }
