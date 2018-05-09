{-|
Module      : Gargantext.Text.Ngrams.PosTagging.CoreNLP
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
{-# LANGUAGE FlexibleContexts  #-}


module Gargantext.Text.Ngrams.PosTagging.CoreNLP
  where

import GHC.Generics
import GHC.Show (Show(..))

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson
import Data.Monoid
import Data.Maybe (isJust)

import Data.Set (Set, fromList, empty)

import Data.Text (Text, splitOn, pack, toLower, unpack)

import Gargantext.Core (Lang(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

import Network.HTTP.Simple

import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class  (MonadIO)
import Data.String.Conversions (ConvertibleStrings)

------------------------------------------------------------------------
data Tag = POS | NER
  deriving (Show, Eq)
------------------------------------------------------------------------
------------------------------------------------------------------------
data POS = NP
         | JJ  | VB
         | CC  | IN | DT
         | NoPos
  deriving (Show, Generic, Eq)

------------------------------------------------------------------------
instance FromJSON POS where
  parseJSON = withText "String" (\x -> pure (pos $ unpack x))
    where
      pos :: [Char] -> POS
      pos "NP"  = NP
      pos "NN"  = NP
      pos "NC"  = NP
      pos "NNS" = NP
      pos "NNP" = NP
      pos "JJ"  = JJ
      pos "ADJ" = JJ
      pos "VB"  = VB
      pos "VBN" = VB
      pos "VBG" = VB
      pos "CC"  = CC
      pos "IN"  = IN
      pos "DT"  = DT
      -- French specific
      pos "P"  = IN
      pos  _    = NoPos

instance ToJSON POS
------------------------------------------------------------------------
data NER = PERSON | ORGANIZATION | LOCATION | NoNER
  deriving (Show, Generic)
------------------------------------------------------------------------
instance FromJSON NER where
  parseJSON = withText "String" (\x -> pure (ner $ unpack x))
    where
      ner :: [Char] -> NER
      ner "PERSON"       = PERSON
      ner "ORGANIZATION" = ORGANIZATION
      ner "LOCATION"     = LOCATION
      ner  _             = NoNER

instance ToJSON NER
------------------------------------------------------------------------
data Token = Token { _tokenIndex                :: Int
                   , _tokenWord                 :: Text
                   , _tokenOriginalText         :: Text
                   , _tokenLemma                :: Text
                   , _tokenCharacterOffsetBegin :: Int
                   , _tokenCharacterOffsetEnd   :: Int
                   , _tokenPos                  :: Maybe POS
                   , _tokenNer                  :: Maybe NER
                   , _tokenBefore               :: Maybe Text
                   , _tokenAfter                :: Maybe Text
                   } deriving (Show, Generic)
$(deriveJSON (unPrefix "_token") ''Token)
------------------------------------------------------------------------
data NgramsTag  = NgramsTag { _my_token_word :: [Text]
                            , _my_token_stem :: Set Text
                            , _my_token_pos  :: Maybe POS
                            , _my_token_ner  :: Maybe NER
                            } deriving (Show)
------------------------------------------------------------------------
tokens2ngramsTags :: [Token] -> [NgramsTag]
tokens2ngramsTags ts = select $ map ngramsTag ts
------------------------------------------------------------------------
ngramsTag :: Token -> NgramsTag
ngramsTag (Token _ _ w s _ _ p n _ _) = NgramsTag w' s' p n
  where
    w' = split w
    s' = fromList (split s)
    split = splitOn (pack " ") . toLower

select :: [NgramsTag] -> [NgramsTag]
select xs = filter isNgrams xs
    where
      isNgrams (NgramsTag _ _ p n) = isJust p || isJust n

instance Monoid NgramsTag where
  mempty = NgramsTag [] empty Nothing Nothing

  mappend (NgramsTag w1 s1 p1 n1) (NgramsTag w2 s2 p2 _)
         = NgramsTag (w1 <> w2) (s1 <> s2) p3 n1
          where
            p3 = case (p1,p2) of
                   (Just JJ, Just NP) -> Just NP
                   (Just VB, Just NP) -> Just NP
                   _                  -> p1

  mconcat = foldl mappend mempty

------------------------------------------------------------------------
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


corenlp' :: ( MonadThrow m, MonadIO m, FromJSON a
            , ConvertibleStrings p ByteString) =>
            Lang -> p -> m (Response a)
corenlp' lang txt = do
    let properties = case lang of
            EN -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}"
            -- FR -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}"
            FR -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\", \"pos.model\":\"edu/stanford/nlp/models/pos-tagger/french/french.tagger\", \"tokenize.language\":\"fr\", \"outputFormat\": \"json\"}"
    url <- parseRequest $ "POST http://localhost:9000/?properties=" <> properties
    let request = setRequestBodyLBS (cs txt) url
    httpJSON request

corenlpRaw :: Lang -> Text -> IO Value
corenlpRaw lang txt = do
  response <- corenlp' lang txt
  pure (getResponseBody response)


corenlp :: Lang -> Text -> IO Sentences
corenlp lang txt = do
  response <- corenlp' lang txt
  pure (getResponseBody response)

-- | parseWith
-- Part Of Speech example
-- parseWith  _tokenPos    "Hello world."
-- == [[("``","``"),("Hello","UH"),("world","NN"),(".","."),("''","''")]]

-- Named Entity Recognition example
-- parseWith  _tokenNer     "Hello world of Peter."
-- [[("``","O"),("Hello","O"),("world","O"),("of","O"),("Peter","PERSON"),(".","O"),("''","O")]]
tokenWith :: (Token -> t) -> Lang -> Text -> IO [[(Text, t)]]
tokenWith f lang s = map (map (\t -> (_tokenWord t, f t))) 
                  <$> map _sentenceTokens
                  <$> _sentences 
                  <$> corenlp lang s


