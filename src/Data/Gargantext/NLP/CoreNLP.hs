{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Gargantext.NLP.CoreNLP where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Data.Gargantext.Prelude
import Data.Gargantext.Utils.Prefix (unPrefix)
import Data.Text (Text)

data Token = Token { _tokenIndex                :: Int
                   , _tokenWord                 :: Text
                   , _tokenOriginalText         :: Text
                   , _tokenLemma                :: Text
                   , _tokenCharacterOffsetBegin :: Int
                   , _tokenCharacterOffsetEnd   :: Int
                   , _tokenPos                  :: Text
                   , _tokenNer                  :: Text
                   , _tokenBefore               :: Text
                   , _tokenAfter                :: Text
                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "_token") ''Token)

data Sentence  = Sentence { _sentenceIndex :: Int
                          , _sentenceToken :: [Token]
                          } deriving (Show, Generic)

$(deriveJSON (unPrefix "_sentence") ''Sentence)

data Properties = Properties { _propertiesAnnotators  :: Text
                             , _propertiesOutputFormat :: Text
                             } deriving (Show, Generic)

$(deriveJSON (unPrefix "_properties") ''Properties)

data Sentences = Sentences { sentences :: [Sentence]}
  deriving (Show, Generic)
instance ToJSON Sentences

-- API Client configuration

-- Example of Client Request :
-- wget --post-data 'Alexandre Grothendieck is a mathematician who lived in France which is a european country. There is another sentence  here.' 'localhost:9000/?properties={"annotators": "tokenize,ssplit,pos,ner", "outputFormat": "json"}' -O

-- the result is Sentence as a JSON
-- {"sentences":[{"index":0,"tokens":[{"index":1,"word":"Alexandre","originalText":"Alexandre","lemma":"Alexandre","characterOffsetBegin":0,"characterOffsetEnd":9,"pos":"NNP","ner":"PERSON","before":"","after":" "},{"index":2,"word":"Grothendieck","originalText":"Grothendieck","lemma":"Grothendieck","characterOffsetBegin":10,"characterOffsetEnd":22,"pos":"NNP","ner":"PERSON","before":" ","after":" "},{"index":3,"word":"is","originalText":"is","lemma":"be","characterOffsetBegin":23,"characterOffsetEnd":25,"pos":"VBZ","ner":"O","before":" ","after":" "},{"index":4,"word":"a","originalText":"a","lemma":"a","characterOffsetBegin":26,"characterOffsetEnd":27,"pos":"DT","ner":"O","before":" ","after":" "},{"index":5,"word":"mathematician","originalText":"mathematician","lemma":"mathematician","characterOffsetBegin":28,"characterOffsetEnd":41,"pos":"NN","ner":"O","before":" ","after":" "},{"index":6,"word":"who","originalText":"who","lemma":"who","characterOffsetBegin":42,"characterOffsetEnd":45,"pos":"WP","ner":"O","before":" ","after":" "},{"index":7,"word":"lived","originalText":"lived","lemma":"live","characterOffsetBegin":46,"characterOffsetEnd":51,"pos":"VBD","ner":"O","before":" ","after":" "},{"index":8,"word":"in","originalText":"in","lemma":"in","characterOffsetBegin":52,"characterOffsetEnd":54,"pos":"IN","ner":"O","before":" ","after":" "},{"index":9,"word":"France","originalText":"France","lemma":"France","characterOffsetBegin":55,"characterOffsetEnd":61,"pos":"NNP","ner":"LOCATION","before":" ","after":" "},{"index":10,"word":"which","originalText":"which","lemma":"which","characterOffsetBegin":62,"characterOffsetEnd":67,"pos":"WDT","ner":"O","before":" ","after":" "},{"index":11,"word":"is","originalText":"is","lemma":"be","characterOffsetBegin":68,"characterOffsetEnd":70,"pos":"VBZ","ner":"O","before":" ","after":" "},{"index":12,"word":"a","originalText":"a","lemma":"a","characterOffsetBegin":71,"characterOffsetEnd":72,"pos":"DT","ner":"O","before":" ","after":" "},{"index":13,"word":"european","originalText":"european","lemma":"european","characterOffsetBegin":73,"characterOffsetEnd":81,"pos":"JJ","ner":"O","before":" ","after":" "},{"index":14,"word":"country","originalText":"country","lemma":"country","characterOffsetBegin":82,"characterOffsetEnd":89,"pos":"NN","ner":"O","before":" ","after":""},{"index":15,"word":".","originalText":".","lemma":".","characterOffsetBegin":89,"characterOffsetEnd":90,"pos":".","ner":"O","before":"","after":" "}]},{"index":1,"tokens":[{"index":1,"word":"There","originalText":"There","lemma":"there","characterOffsetBegin":91,"characterOffsetEnd":96,"pos":"EX","ner":"O","before":" ","after":" "},{"index":2,"word":"is","originalText":"is","lemma":"be","characterOffsetBegin":97,"characterOffsetEnd":99,"pos":"VBZ","ner":"O","before":" ","after":" "},{"index":3,"word":"another","originalText":"another","lemma":"another","characterOffsetBegin":100,"characterOffsetEnd":107,"pos":"DT","ner":"O","before":" ","after":" "},{"index":4,"word":"sentence","originalText":"sentence","lemma":"sentence","characterOffsetBegin":108,"characterOffsetEnd":116,"pos":"NN","ner":"O","before":" ","after":"  "},{"index":5,"wo


type API = "" :> QueryParam "properties" Properties :> ReqBody '[JSON] String :> Post '[JSON] String

corenlp :: Maybe Properties -> Text -> ClientM Sentence
corenlp p t = client api

-- text2nlp :: Text -> ClientM 

api :: Proxy API
api = Proxy


-- corenlp t = client api

-- | URI scheme to use
--data Scheme =
--    Http  -- ^ http://
--  | Https -- ^ https://
--
---- | Simple data type to represent the target of HTTP requests
----   for servant's automatically-generated clients.
--data BaseUrl = BaseUrl
--  { baseUrlScheme :: Scheme -- ^ URI scheme to use
--  , baseUrlHost :: String   -- ^ host (eg "haskell.org")
--  , baseUrlPort :: Int      -- ^ port (eg 80)
--  , baseUrlPath :: String   -- ^ path (eg "/a/b/c")
--  }
--

queries :: ClientM (Text, Properties)
queries = do
    let text = "Alexandre Grothendieck is free even in a sentence." 
    let prop = Properties "tokenize,ssplit,pos,ner" "json"
    return (text, prop)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 9000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right x -> do
      print x







