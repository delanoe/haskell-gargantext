{-|
Module      : Gargantext.Utils.JohnSnowNLP
Description : PosTagging module using Stanford java REST API
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Utils.JohnSnowNLP where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Aeson (encode, ToJSON, toJSON, FromJSON, parseJSON, Value(..), (.:), (.:?))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson.TH (deriveJSON)
import qualified Data.List.Safe as LS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text hiding (map, group, filter, concat, zip)
import Network.HTTP.Simple (parseRequest, httpJSON, setRequestBodyLBS, getResponseBody, Response)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (POS(..))
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types
import Gargantext.Core.Utils.Prefix (unPrefix)


data JSSpell = JSPOS Lang | JSLemma Lang
  deriving (Show)

instance ToJSON JSSpell where
  toJSON (JSPOS EN)    = "en.pos"
  toJSON (JSPOS FR)    = "fr.pos"
  toJSON (JSPOS All)   = "pos"
  toJSON (JSLemma EN)  = "en.lemma"
  toJSON (JSLemma FR)  = "fr.lemma"
  toJSON (JSLemma All) = "lemma"

instance FromJSON JSSpell where
  parseJSON (String "en.pos")   = pure $ JSPOS EN
  parseJSON (String "fr.pos")   = pure $ JSPOS FR
  parseJSON (String "pos")      = pure $ JSPOS All
  parseJSON (String "en.lemma") = pure $ JSLemma EN
  parseJSON (String "fr.lemma") = pure $ JSLemma FR
  parseJSON (String "lemma")    = pure $ JSLemma All
  parseJSON s =
    prependFailure "parsing spell failed, "
    (typeMismatch "Spell" s)

data JSRequest =
  JSRequest { _jsRequest_data     :: !Text
            , _jsRequest_format   :: !Text
            , _jsRequest_grouping :: !(Maybe Text)
            , _jsRequest_spell    :: !JSSpell }
  deriving (Show)

-- "spell" options:
-- https://nlu.johnsnowlabs.com/docs/en/spellbook

deriveJSON (unPrefix "_jsRequest_") ''JSRequest

-- | JohnSnow NLP works via asynchronous tasks: send a query and get a
-- task in response. One must poll for task status and then get it's
-- result.
data JSAsyncTask =
  JSAsyncTask { _jsAsyncTask_uuid :: !Text }
  deriving (Show)

deriveJSON (unPrefix "_jsAsyncTask_") ''JSAsyncTask

-- | Task status.
data JSAsyncTaskStatus =
  JSAsyncTaskStatus { _jsAsyncTaskStatus_code    :: !Text
                    , _jsAsyncTaskStatus_message :: !(Maybe Text) }
  deriving (Show)

taskReady :: JSAsyncTaskStatus -> Bool
taskReady (JSAsyncTaskStatus { .. }) = _jsAsyncTaskStatus_code == "success"

--deriveJSON (unPrefix "_jsAsyncTaskStatus_") ''JSAsyncTaskStatus
instance FromJSON JSAsyncTaskStatus where
  parseJSON (Object v) = do
    status <- v .: "status"
    code <- status .: "code"
    message <- status .:? "message"
    pure $ JSAsyncTaskStatus { _jsAsyncTaskStatus_code = code
                             , _jsAsyncTaskStatus_message = message }
  parseJSON s =
    prependFailure "parsing status failed"
    (typeMismatch "status" s)

-- | Response for our query. The `Maybe` types are here because we
-- combine 2 types of responses into one: `pos` and `lemma`.
data JSAsyncTaskResponse =
  JSAsyncTaskResponse { _jsAsyncTaskResponse_index    :: Map Text Int
                      , _jsAsyncTaskResponse_document :: Map Text Text
                      , _jsAsyncTaskResponse_sentence :: Map Text [Text]
                      , _jsAsyncTaskResponse_lem      :: Maybe (Map Text [Text])
                      , _jsAsyncTaskResponse_pos      :: Maybe (Map Text [POS])
                      , _jsAsyncTaskResponse_token    :: Map Text [Text] }
  deriving (Show)

deriveJSON (unPrefix "_jsAsyncTaskResponse_") ''JSAsyncTaskResponse
makeLenses ''JSAsyncTaskResponse

-- | We need to combine 2 responses: `pos` and `lemma` spells.
jsAsyncTaskResponseToSentences :: JSAsyncTaskResponse -> JSAsyncTaskResponse -> PosSentences
jsAsyncTaskResponseToSentences jsPos jsLemma =
  PosSentences { _sentences }
  where
    _sentences = Map.elems $ Map.mapWithKey mapSentence (jsPos ^. jsAsyncTaskResponse_sentence)
    mapSentence idx sentence = Sentence { _sentenceIndex = sIndex
                                         , _sentenceTokens = sTokens }
      where
        sIndex = Map.findWithDefault (-1) idx (jsPos ^. jsAsyncTaskResponse_index)
        lemmas = fromMaybe [] $
          if Just sentence == Map.lookup idx (jsLemma ^. jsAsyncTaskResponse_sentence) then
            Map.lookup idx $ fromMaybe Map.empty (jsLemma ^. jsAsyncTaskResponse_lem)
          else
            Nothing
        sTokens = imap mapPosToken $ zip (Map.findWithDefault [] idx $ fromMaybe Map.empty (jsPos ^. jsAsyncTaskResponse_pos))
                                         (Map.findWithDefault [] idx (jsPos ^. jsAsyncTaskResponse_token))
        mapPosToken idx' (pos, token) = Token { _tokenIndex = -1
                                              , _tokenWord = token
                                              , _tokenOriginalText = ""
                                              , _tokenLemma = fromMaybe "" $ (LS.!!) lemmas idx'
                                              , _tokenCharacterOffsetBegin = -1
                                              , _tokenCharacterOffsetEnd = -1
                                              , _tokenPos = Just pos
                                              , _tokenNer = Nothing
                                              , _tokenBefore = Nothing
                                              , _tokenAfter = Nothing }

-----------------------------------------------------

jsRequest :: Text -> JSSpell -> IO JSAsyncTask
jsRequest t s = do
  url <- parseRequest $ "POST http://localhost:5000/api/results"
  let jsReq = JSRequest { _jsRequest_data     = t
                        , _jsRequest_format   = "text"
                        , _jsRequest_grouping = Nothing
                        , _jsRequest_spell    = s }
  let request = setRequestBodyLBS (encode jsReq) url
  task <- httpJSON request :: IO (Response JSAsyncTask)
  pure $ getResponseBody task

jsTaskStatus :: JSAsyncTask -> IO JSAsyncTaskStatus
jsTaskStatus (JSAsyncTask uuid) = do
  url <- parseRequest $ unpack $ "GET http://localhost:5000/api/results/" <> uuid <> "/status"
  status <- httpJSON url
  pure $ getResponseBody status

jsTaskResponse :: JSAsyncTask -> IO JSAsyncTaskResponse
jsTaskResponse (JSAsyncTask uuid) = do
  url <- parseRequest $ unpack $ "GET http://localhost:5000/api/results/" <> uuid
  result <- httpJSON url
  pure $ getResponseBody result

waitForJsTask :: JSAsyncTask -> IO JSAsyncTaskResponse
waitForJsTask jsTask = wait' 0
  where
    wait' :: Int -> IO JSAsyncTaskResponse
    wait' counter = do
      status <- jsTaskStatus jsTask
      if taskReady status then
        jsTaskResponse jsTask
      else
        if counter > 60 then
          panic "[waitForJsTask] waited for 1 minute and still no answer from JohnSnow NLP"
        else do
          printDebug "[waitForJsTask] task not ready, waiting" counter
          _ <- threadDelay $ 1000000*1
          wait' $ counter + 1

getPosTagAndLems :: Lang -> Text -> IO PosSentences
getPosTagAndLems l t = do
  jsPosTask <- jsRequest t (JSPOS l)
  jsLemmaTask <- jsRequest t (JSLemma l)

  -- wait for both tasks
  jsPos <- waitForJsTask jsPosTask
  jsLemma <- waitForJsTask jsLemmaTask

  printDebug "[getPosTagAndLems] sentences" $ jsAsyncTaskResponseToSentences jsPos jsLemma

  pure $ PosSentences []

