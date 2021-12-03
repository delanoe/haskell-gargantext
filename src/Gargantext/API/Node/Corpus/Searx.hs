{-# LANGUAGE TemplateHaskell #-}

module Gargantext.API.Node.Corpus.Searx where



import Control.Lens (view)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Either (Either(..))
import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Tuple.Select (sel1, sel2, sel3)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Prelude as Prelude
import Protolude (encodeUtf8, Text)
import Gargantext.Prelude
import Gargantext.Prelude.Config

import Gargantext.Core (Lang(..))
import qualified Gargantext.Core.Text.Corpus.API as API
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Action.Flow.Types (FlowCmdM)
import Gargantext.Database.Admin.Config ()
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Database.Admin.Types.Node (CorpusId)
import Gargantext.Database.Prelude (hasConfig)
import Gargantext.Database.Query.Table.Node (defaultList)


langToSearx :: Lang -> Text
langToSearx EN = "en-US"
langToSearx FR = "fr-FR"
langToSearx All = "en-US"

data SearxResult = SearxResult
  { _sr_url           :: Text
  , _sr_title         :: Text
  , _sr_content       :: Maybe Text
  , _sr_engine        :: Text
  , _sr_score         :: Double
  , _sr_category      :: Text
  , _sr_pretty_url    :: Text
  , _sr_publishedDate :: Text   -- "Nov 19, 2021"
  , _sr_pubdate       :: Text  -- "2021-11-19 02:12:00+0000"
  }
  deriving (Show, Eq, Generic)
--  , _sr_parsed_url
--  , _sr_engines
--  , _sr_positions

$(deriveJSON (unPrefix "_sr_") ''SearxResult)

data SearxResponse = SearxResponse
  { _srs_query                :: Text
  , _srs_number_of_results    :: Int
  , _srs_results              :: [SearxResult] }
  deriving (Show, Eq, Generic)
-- , _srs_answers
-- , _srs_corrections
-- , _srs_infoboxes
--  , _srs_suggestions          :: [Text]
--  , _srs_unresponsive_engines :: [Text] }

$(deriveJSON (unPrefix "_srs_") ''SearxResponse)

data FetchSearxParams = FetchSearxParams
  { _fsp_language :: Lang
  , _fsp_manager :: Manager
  , _fsp_pageno  :: Int
  , _fsp_query   :: Text
  , _fsp_url     :: Text
  }

fetchSearxPage :: FetchSearxParams -> IO (Either Prelude.String SearxResponse)
fetchSearxPage (FetchSearxParams { _fsp_language
                                 , _fsp_manager
                                 , _fsp_pageno
                                 , _fsp_query
                                 , _fsp_url }) = do
  -- searx search API:
  -- https://searx.github.io/searx/dev/search_api.html?highlight=json
  req <- parseRequest $ T.unpack _fsp_url
  let request = urlEncodedBody
        [ --("category_general", "1")
          ("q", encodeUtf8 _fsp_query)
        , ("categories", "news")  -- https://gitlab.iscpif.fr/gargantext/haskell-gargantext/issues/70#note_3976
        , ("pageno", encodeUtf8 $ T.pack $ show _fsp_pageno)
          --, ("time_range", "None")
        , ("language", encodeUtf8 $ langToSearx _fsp_language)
        , ("format", "json")
        ] req
  res <- httpLbs request _fsp_manager
  let dec = Aeson.eitherDecode $ responseBody res :: (Either Prelude.String SearxResponse)
  pure dec

-- TODO Make an async task out of this?
triggerSearxSearch :: (MonadBase IO m, FlowCmdM env err m)
            => CorpusId
            -> API.Query
            -> Lang
            -> m ()
triggerSearxSearch cId q l = do
  printDebug "[triggerSearxSearch] cId" cId
  printDebug "[triggerSearxSearch] q" q
  printDebug "[triggerSearxSearch] l" l
  cfg <- view hasConfig
  let surl = _gc_frame_searx_url cfg
  printDebug "[triggerSearxSearch] surl" surl
  listId <- defaultList cId
  printDebug "[triggerSearxSearch] listId" listId

  manager <- liftBase $ newManager tlsManagerSettings
  res <- liftBase $ fetchSearxPage $ FetchSearxParams { _fsp_language = l
                                                      , _fsp_manager = manager
                                                      , _fsp_pageno = 1
                                                      , _fsp_query = q
                                                      , _fsp_url = surl }
   
  printDebug "[triggerSearxSearch] res" res

  _ <- case res of
   Left _ -> pure ()
   Right (SearxResponse { _srs_results }) -> do
     let docs = hyperdataDocumentFromSearxResult <$> _srs_results
     printDebug "[triggerSearxSearch] docs" docs

  pure ()

hyperdataDocumentFromSearxResult :: SearxResult -> Either T.Text HyperdataDocument
hyperdataDocumentFromSearxResult (SearxResult { _sr_content, _sr_engine, _sr_pubdate, _sr_title }) = do
  let mDate = parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack _sr_pubdate) :: Maybe Day
  let mGregorian = toGregorian <$> mDate
  Right HyperdataDocument { _hd_bdd = Just "Searx"
                          , _hd_doi = Nothing
                          , _hd_url = Nothing
                          , _hd_uniqId = Nothing
                          , _hd_uniqIdBdd = Nothing
                          , _hd_page = Nothing
                          , _hd_title = Just _sr_title
                          , _hd_authors = Nothing
                          , _hd_institutes = Nothing
                          , _hd_source = Just _sr_engine
                          , _hd_abstract = _sr_content
                          , _hd_publication_date = Just _sr_pubdate
                          , _hd_publication_year = fromIntegral <$> sel1 <$> mGregorian
                          , _hd_publication_month = sel2 <$> mGregorian
                          , _hd_publication_day = sel3 <$> mGregorian
                          , _hd_publication_hour = Nothing
                          , _hd_publication_minute = Nothing
                          , _hd_publication_second = Nothing
                          , _hd_language_iso2 = Just $ T.pack $ show EN }

