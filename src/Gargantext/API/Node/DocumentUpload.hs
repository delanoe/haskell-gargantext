{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Gargantext.API.Node.DocumentUpload where

import Control.Lens (makeLenses, view)
import Data.Aeson
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Servant
import qualified Data.Text as T

import Gargantext.API.Admin.EnvTypes (GargJob(..), Env)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Job (jobLogSuccess)
import Gargantext.API.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.Date (dateSplit)
import Gargantext.Core.Utils.Prefix (unCapitalize, dropPrefix)
import Gargantext.Database.Action.Flow.Types
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Database.Action.Flow (insertMasterDocs)
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import qualified Gargantext.Database.Query.Table.Node.Document.Add  as Doc  (add)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType')
import Gargantext.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Corpus (HyperdataCorpus(..))
import Gargantext.Utils.Jobs (serveJobsAPI, jobHandleLogger)


data DocumentUpload = DocumentUpload
  { _du_abstract :: T.Text
  , _du_authors  :: T.Text
  , _du_sources  :: T.Text
  , _du_title    :: T.Text
  , _du_date     :: T.Text
  , _du_language :: T.Text
  }
  deriving (Generic)

$(makeLenses ''DocumentUpload)

instance ToSchema DocumentUpload
instance FromJSON DocumentUpload
  where
    parseJSON = genericParseJSON
            ( defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = unCapitalize . dropPrefix "_du_"
                            , omitNothingFields = True
                            }
            )
instance ToJSON DocumentUpload
  where
    toJSON = genericToJSON
           ( defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = unCapitalize . dropPrefix "_du_"
                            , omitNothingFields = True
                            }
           )

type API = Summary " Document upload"
           :> "document"
           :> "upload"
           :> "async"
           :> AsyncJobs JobLog '[JSON] DocumentUpload JobLog

api :: UserId -> NodeId -> ServerT API (GargM Env GargError)
api uId nId =
  serveJobsAPI UploadDocumentJob $ \jHandle q -> do
      documentUploadAsync uId nId q (jobHandleLogger jHandle)

documentUploadAsync :: (FlowCmdM env err m)
               => UserId
               -> NodeId
               -> DocumentUpload
               -> (JobLog -> m ())
               -> m JobLog
documentUploadAsync _uId nId doc logStatus = do
  let jl = JobLog { _scst_succeeded = Just 0
                  , _scst_failed    = Just 0
                  , _scst_remaining = Just 1
                  , _scst_events    = Just [] }
  logStatus jl
  _docIds <- documentUpload nId doc
  -- printDebug "documentUploadAsync" docIds
  pure $ jobLogSuccess jl



documentUpload :: (FlowCmdM env err m)
               => NodeId
               -> DocumentUpload
               -> m [DocId]
documentUpload nId doc = do
  mcId <- getClosestParentIdByType' nId NodeCorpus
  let cId = case mcId of
        Just c  -> c
        Nothing -> panic $ T.pack $ "[G.A.N.DU] Node has no corpus parent: " <> show nId

  (theFullDate, (year, month, day)) <- liftBase $ dateSplit EN
                                                        $ Just
                                                        $ view du_date doc <> "T:0:0:0"

  let hd = HyperdataDocument { _hd_bdd = Nothing
                             , _hd_doi = Nothing
                             , _hd_url = Nothing
                             , _hd_uniqId = Nothing
                             , _hd_uniqIdBdd = Nothing
                             , _hd_page = Nothing
                             , _hd_title = Just $ if view du_title doc == "" then T.take 50 (view du_abstract doc) else view du_title doc
                             , _hd_authors = Just $ view du_authors doc
                             , _hd_institutes = Nothing
                             , _hd_source             = Just $ view du_sources doc
                             , _hd_abstract           = Just $ view du_abstract doc
                             , _hd_publication_date   = fmap (T.pack . show) theFullDate
                             , _hd_publication_year   = year
                             , _hd_publication_month  = month
                             , _hd_publication_day    = day
                             , _hd_publication_hour   = Nothing
                             , _hd_publication_minute = Nothing
                             , _hd_publication_second = Nothing
                             , _hd_language_iso2 = Just $ view du_language doc }

  docIds <- insertMasterDocs (Nothing :: Maybe HyperdataCorpus) (Multi EN) [hd]
  _ <- Doc.add cId docIds
  pure docIds
