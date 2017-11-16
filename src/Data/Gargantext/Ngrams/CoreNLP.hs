{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Gargantext.Ngrams.CoreNLP where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import GHC.Generics

import Data.Gargantext.Prelude
import Data.Gargantext.Utils.Prefix (unPrefix)
import Data.Text (Text)

import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple


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

data Sentences = Sentences { sentences :: [Sentence]}
  deriving (Show, Generic)
instance ToJSON Sentences
instance FromJSON Sentences


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


corenlpPretty :: String -> IO ()
corenlpPretty txt = do
    url <- parseRequest "POST http://localhost:9000/?properties={\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}" 
    let request = setRequestBodyJSON txt url
    response <- httpJSON request

--    putStrLn $ "The status code was: " ++
--               show (getResponseStatusCode response)
--    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Sentences)

corenlp :: String -> IO Sentences
corenlp txt = do
    url <- parseRequest "POST http://localhost:9000/?properties={\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}"
    let request = setRequestBodyJSON txt url
    response <- httpJSON request
    pure (getResponseBody response :: Sentences)

-- | parseWith
-- Part Of Speech example
-- parseWith  _tokenPos    "Hello world."
-- == [[("``","``"),("Hello","UH"),("world","NN"),(".","."),("''","''")]]

-- Named Entity Recognition example
-- parseWith  _tokenNer     "Hello world of Peter."
-- [[("``","O"),("Hello","O"),("world","O"),("of","O"),("Peter","PERSON"),(".","O"),("''","O")]]
tokenWith :: (Token -> t) -> String -> IO [[(Text, t)]]
tokenWith f s = pm (pm (\t -> (_tokenWord t, f t))) <$> pm _sentenceTokens <$> sentences <$> corenlp s



