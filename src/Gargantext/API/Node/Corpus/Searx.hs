{-# LANGUAGE TemplateHaskell #-}

module Gargantext.API.Node.Corpus.Searx where

import Control.Lens (view)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Protolude (encodeUtf8, Text)
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
  , _sr_content    :: Text
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

  res <- liftBase $ do
    manager <- newManager tlsManagerSettings
    req <- parseRequest $ T.unpack surl
    let request = urlEncodedBody [ ("category_general", "1")
                                 , ("q", encodeUtf8 q)
                                 , ("pageno", "1")
                                 , ("time_range", "None")
                                 , ("language", "en-US")  -- TODO
                                 , ("format", "json")] req
    httpLbs request manager
  let dec = Aeson.decode $ responseBody res :: (Maybe SearxResponse)
  printDebug "[triggerSearxSearch] dec" dec

  pure ()


