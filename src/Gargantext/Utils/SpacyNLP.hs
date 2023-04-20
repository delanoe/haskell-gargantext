{-|
Module      : Gargantext.Utils.SpacyNLP
Description : John Snow NLP API connexion
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Spacy ecosystem: https://github.com/explosion/spaCy

Server to be used: https://gitlab.iscpif.fr/gargantext/spacy-server

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Utils.SpacyNLP where

import Control.Lens
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON)
import Data.Text hiding (map, group, filter, concat, zip)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types
import Gargantext.Core.Types (POS(..), NER(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude
import Network.HTTP.Simple (parseRequest, httpJSON, setRequestBodyLBS, getResponseBody, Response)
import Network.URI (URI(..))


data SpacyData = SpacyData { _spacy_data :: ![SpacyText]}
  deriving (Show)

data SpacyText = SpacyText { _spacy_text :: !Text
                           , _spacy_tags :: ![SpacyTags]
                           } deriving (Show)
data SpacyTags =
  SpacyTags { _spacyTags_text :: !Text
            , _spacyTags_text_with_ws :: !Text
            , _spacyTags_whitespace :: !Text
            , _spacyTags_head :: !Text
            , _spacyTags_head_index :: !Int
            , _spacyTags_left_edge :: !Text
            , _spacyTags_right_edge :: !Text
            , _spacyTags_index :: Int
            , _spacyTags_ent_type :: !NER
            , _spacyTags_ent_iob :: !Text
            , _spacyTags_lemma :: !Text
            , _spacyTags_normalized :: !Text
            , _spacyTags_shape :: !Text
            , _spacyTags_prefix :: !Text
            , _spacyTags_suffix :: !Text
            , _spacyTags_is_alpha :: Bool
            , _spacyTags_is_ascii :: Bool
            , _spacyTags_is_digit :: Bool
            , _spacyTags_is_title :: Bool
            , _spacyTags_is_punct :: Bool
            , _spacyTags_is_left_punct :: Bool
            , _spacyTags_is_right_punct :: Bool
            , _spacyTags_is_space :: Bool
            , _spacyTags_is_bracket :: Bool
            , _spacyTags_is_quote :: Bool
            , _spacyTags_is_currency :: Bool
            , _spacyTags_like_url :: Bool
            , _spacyTags_like_num :: Bool
            , _spacyTags_like_email :: Bool
            , _spacyTags_is_oov :: Bool
            , _spacyTags_is_stop :: Bool
            , _spacyTags_pos :: POS
            , _spacyTags_tag :: POS
            , _spacyTags_dep :: !Text
            , _spacyTags_lang :: !Text
            , _spacyTags_prob :: !Int
            , _spacyTags_char_offset :: !Int
            } deriving (Show)


data SpacyRequest = SpacyRequest { _spacyRequest_text :: !Text }
  deriving (Show)

spacyRequest :: URI -> Text -> IO SpacyData
spacyRequest uri txt = do
  req <- parseRequest $ "POST " <> show (uri { uriPath = "/pos" })
  let request = setRequestBodyLBS (encode $ SpacyRequest txt) req
  result <- httpJSON request :: IO (Response SpacyData)
  pure $ getResponseBody result


-- Instances
deriveJSON (unPrefix "_spacy_")        ''SpacyData
deriveJSON (unPrefix "_spacy_")        ''SpacyText
deriveJSON (unPrefix "_spacyTags_")    ''SpacyTags
deriveJSON (unPrefix "_spacyRequest_") ''SpacyRequest

makeLenses ''SpacyData
makeLenses ''SpacyText
makeLenses ''SpacyTags
makeLenses ''SpacyRequest

----------------------------------------------------------------
spacyTagsToToken :: SpacyTags -> Token
spacyTagsToToken st = Token (st ^. spacyTags_index)
                   (st ^. spacyTags_normalized)
                   (st ^. spacyTags_text)
                   (st ^. spacyTags_lemma)
                   (st ^. spacyTags_head_index)
                   (st ^. spacyTags_char_offset)
                   (Just $ st ^. spacyTags_pos)
                   (Just $ st ^. spacyTags_ent_type)
                   (Just $ st ^. spacyTags_prefix)
                   (Just $ st ^. spacyTags_suffix)

spacyDataToPosSentences :: SpacyData -> PosSentences
spacyDataToPosSentences (SpacyData ds) = PosSentences
   $  map (\(i, ts) -> Sentence i ts)
   $ zip [1..]
   $ map (\(SpacyText _ tags)-> map spacyTagsToToken tags) ds

-----------------------------------------------------------------

nlp :: URI -> Lang -> Text -> IO PosSentences
nlp uri _lang txt = spacyDataToPosSentences <$> spacyRequest uri txt
-- nlp _ _ _ = panic "Make sure you have the right model for your lang for spacy Server"
-- nlp FR txt  = spacyDataToPosSentences <$> spacyRequest txt
-- nlp _  _ = panic "Make sure you have the right model for your lang for spacy Server"
