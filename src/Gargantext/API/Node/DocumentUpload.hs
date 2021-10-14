{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Gargantext.API.Node.DocumentUpload where

import Control.Lens (makeLenses, view)
import Data.Aeson
import Data.Swagger (ToSchema)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Generics (Generic)
import Servant
import Servant.Job.Async
import Web.FormUrlEncoded (FromForm)

import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Job (jobLogSuccess)
import Gargantext.API.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Terms (TermType(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Flow (flowDataText, DataText(..))
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType')
import Gargantext.Prelude

data DocumentUpload = DocumentUpload
  { _abstract :: T.Text
  , _authors  :: T.Text
  , _sources  :: T.Text
  , _title    :: T.Text }
  deriving (Generic)

$(makeLenses ''DocumentUpload)

instance FromForm DocumentUpload
instance FromJSON DocumentUpload
instance ToJSON DocumentUpload
instance ToSchema DocumentUpload

type API = Summary " Document upload"
           :> "framecalc"
           :> "async"
           :> AsyncJobs JobLog '[JSON] DocumentUpload JobLog

api :: UserId -> NodeId -> GargServer API
api uId nId =
  serveJobsAPI $
    JobFunction (\q log' -> do
      documentUpload uId nId q (liftBase . log')
    )

documentUpload :: (FlowCmdM env err m)
               => UserId
               -> NodeId
               -> DocumentUpload
               -> (JobLog -> m ())
               -> m JobLog
documentUpload uId nId doc logStatus = do
  let jl = JobLog { _scst_succeeded = Just 0
                  , _scst_failed    = Just 0
                  , _scst_remaining = Just 1
                  , _scst_events    = Just [] }
  logStatus jl

  mcId <- getClosestParentIdByType' nId NodeCorpus
  let cId = case mcId of
        Just c  -> c
        Nothing -> panic $ T.pack $ "[G.A.N.DU] Node has no corpus parent: " <> show nId

  (year, month, day) <- liftBase $ getCurrentTime >>= return . toGregorian . utctDay
  let nowS = T.pack $ show year <> "-" <> show month <> "-" <> show day

  let hd = HyperdataDocument { _hd_bdd = Nothing
                             , _hd_doi = Nothing
                             , _hd_url = Nothing
                             , _hd_uniqId = Nothing
                             , _hd_uniqIdBdd = Nothing
                             , _hd_page = Nothing
                             , _hd_title = Just $ view title doc
                             , _hd_authors = Just $ view authors doc
                             , _hd_institutes = Nothing
                             , _hd_source = Just $ view sources doc
                             , _hd_abstract = Just $ view abstract doc
                             , _hd_publication_date = Just nowS
                             , _hd_publication_year = Just $ fromIntegral year
                             , _hd_publication_month = Just month
                             , _hd_publication_day = Just day
                             , _hd_publication_hour = Nothing
                             , _hd_publication_minute = Nothing
                             , _hd_publication_second = Nothing
                             , _hd_language_iso2 = Just $ T.pack $ show EN }

  _ <- flowDataText (RootId (NodeId uId)) (DataNew [[hd]]) (Multi EN) cId Nothing

  pure $ jobLogSuccess jl
