{-|
Module      : Gargantext.Ngrams.CoreNLP
Description : CoreNLP module
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Ngrams.CoreNLP where

import Data.Aeson.TH (deriveJSON)
import GHC.Generics
import Data.Monoid ((<>))
import GHC.Show (Show(..))

import Gargantext.Types.Main (Language(..))
import Gargantext.Prelude
import Gargantext.Utils.Prefix (unPrefix)
import Data.Text (Text)

import Network.HTTP.Simple


data Token = Token { _tokenIndex                :: Int
                   , _tokenWord                 :: Text
                   , _tokenOriginalText         :: Text
                   , _tokenLemma                :: Text
                   , _tokenCharacterOffsetBegin :: Int
                   , _tokenCharacterOffsetEnd   :: Int
                   , _tokenPos                  :: Text
                   , _tokenNer                  :: Text
                   , _tokenBefore               :: Maybe Text
                   , _tokenAfter                :: Maybe Text
                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "_token") ''Token)

token2text :: Token -> (Text, Text, Text)
token2text (Token _ w _ _ _ _ p n _ _) = (w,p,n)


data Sentence  = Sentence { _sentenceIndex :: Int
                          , _sentenceTokens :: [Token]
                          } deriving (Show, Generic)

$(deriveJSON (unPrefix "_sentence") ''Sentence)

data Properties = Properties { _propertiesAnnotators  :: Text
                             , _propertiesOutputFormat :: Text
                             } deriving (Show, Generic)

$(deriveJSON (unPrefix "_properties") ''Properties)

data Sentences = Sentences { _sentences :: [Sentence]}
  deriving (Show, Generic)

$(deriveJSON (unPrefix "_") ''Sentences)


-- request = 
-- "fr" : {
--                 "tokenize.language" : "fr",
--                 "pos.model" : "edu/stanford/nlp/models/pos-tagger/french/french.tagger",
--                 "parse.model" : "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz",
--                 // dependency parser
--                 "depparse.model" : "edu/stanford/nlp/models/parser/nndep/UD_French.gz",
--                 "depparse.language" : "french",
--                 "ner.model":  DATA_ROOT+"/eunews.fr.crf.gz",
--                 "ssplit.newlineIsSentenceBreak": "always" 
--             },
-- 


corenlp :: Language -> Text -> IO Sentences
corenlp lang txt = do
    let properties = case lang of
            EN -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}"
            -- FR -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}"
            FR -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\", \"pos.model\":\"edu/stanford/nlp/models/pos-tagger/french/french.tagger\", \"tokenize.language\":\"fr\", \"outputFormat\": \"json\"}"
    url <- parseRequest $ "POST http://localhost:9000/?properties=" <> properties
    let request = setRequestBodyLBS (cs txt) url
    response <- httpJSON request
    pure (getResponseBody response :: Sentences)

-- | parseWith
-- Part Of Speech example
-- parseWith  _tokenPos    "Hello world."
-- == [[("``","``"),("Hello","UH"),("world","NN"),(".","."),("''","''")]]

-- Named Entity Recognition example
-- parseWith  _tokenNer     "Hello world of Peter."
-- [[("``","O"),("Hello","O"),("world","O"),("of","O"),("Peter","PERSON"),(".","O"),("''","O")]]
tokenWith :: (Token -> t) -> Language -> Text -> IO [[(Text, t)]]
tokenWith f lang s = map (map (\t -> (_tokenWord t, f t))) <$> map _sentenceTokens <$> _sentences <$> corenlp lang s



