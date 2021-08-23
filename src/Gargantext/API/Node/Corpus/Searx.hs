{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Gargantext.API.Node.Corpus.Searx where

import Control.Lens (view)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Prelude as Prelude
import Protolude (encodeUtf8, Text, Either)
import Gargantext.Prelude
import Gargantext.Prelude.Config

import Gargantext.Core (Lang(..))
import qualified Gargantext.Core.Text.Corpus.API as API
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Action.Flow (FlowCmdM)
import Gargantext.Database.Admin.Types.Node (CorpusId)
import Gargantext.Database.Prelude (hasConfig)


data SearxResult = SearxResult
  { _sr_url        :: Text
  , _sr_title      :: Text
  , _sr_content    :: Maybe Text
  , _sr_engine     :: Text
  , _sr_score      :: Double
  , _sr_category   :: Text
  , _sr_pretty_url :: Text }
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
  { _fsp_manager :: Manager
  , _fsp_pageno  :: Int
  , _fsp_query   :: Text
  , _fsp_url     :: Text
  }

fetchSearxPage :: FetchSearxParams -> IO (Either Prelude.String SearxResponse)
fetchSearxPage (FetchSearxParams { _fsp_manager
                                 , _fsp_pageno
                                 , _fsp_query
                                 , _fsp_url }) = do
  -- searx search API:
  -- https://searx.github.io/searx/dev/search_api.html?highlight=json
  req <- parseRequest $ T.unpack _fsp_url
  let request = urlEncodedBody
        [ --("category_general", "1")
          ("q", encodeUtf8 _fsp_query)
        , ("pageno", encodeUtf8 $ T.pack $ show _fsp_pageno)
          --, ("time_range", "None")
            --, ("language", "en-US")  -- TODO
        , ("format", "json")
        ] req
  res <- httpLbs request _fsp_manager
  let dec = Aeson.eitherDecode $ responseBody res :: (Either Prelude.String SearxResponse)
  pure dec

triggerSearxSearch :: (MonadBase IO m, FlowCmdM env err m)
            => CorpusId
            -> API.Query
            -> Lang
            -> m ()

triggerSearxSearch cid q l = do
  printDebug "[triggerSearxSearch] cid" cid
  printDebug "[triggerSearxSearch] q" q
  printDebug "[triggerSearxSearch] l" l
  cfg <- view hasConfig
  let surl = _gc_frame_searx_url cfg
  printDebug "[triggerSearxSearch] surl" surl

  manager <- liftBase $ newManager tlsManagerSettings
  res <- liftBase $ fetchSearxPage $ FetchSearxParams { _fsp_manager = manager
                                                      , _fsp_pageno = 1
                                                      , _fsp_query = q
                                                      , _fsp_url = surl }
   
  printDebug "[triggerSearxSearch] res" res

  pure ()
