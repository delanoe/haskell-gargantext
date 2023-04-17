{-|
Module      : Gargantext.Core.Text.Terms.Multi.PosTagging
Description : PosTagging module using Stanford java REST API
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

In corpus linguistics, part-of-speech tagging (POS tagging or PoS
tagging or POST), also called grammatical tagging or word-category
disambiguation, is the process of marking up a word in a text (corpus)
as corresponding to a particular part of speech,[1] based on both its
definition and its context—i.e., its relationship with adjacent and
related words in a phrase, sentence, or paragraph. A simplified form of
this is commonly taught to school-age children, in the identification of
words as nouns, verbs, adjectives, adverbs, etc.

Source: https://en.wikipedia.org/wiki/Part-of-speech_tagging
-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Core.Text.Terms.Multi.PosTagging
  where

import Data.Aeson
import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import Data.Set (fromList)
import Data.Text (Text, splitOn, pack, toLower)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types
import Gargantext.Core.Types
import Gargantext.Prelude
import Network.HTTP.Simple
import Network.URI (URI(..))

-- import qualified Gargantext.Utils.SpacyNLP as SpacyNLP



------------------------------------------------------------------------
------------------------------------------------------------------------
tokens2tokensTags :: [Token] -> [TokenTag]
tokens2tokensTags ts = filter' $ map tokenTag ts
------------------------------------------------------------------------
tokenTag :: Token -> TokenTag
tokenTag (Token { .. }) = TokenTag { _my_token_word = w'
                                   , _my_token_lemma = l'
                                   , _my_token_pos = _tokenPos
                                   , _my_token_ner = _tokenNer }
  where
    w' = split _tokenWord
    l' = fromList (split _tokenLemma)
    split = splitOn (pack " ") . toLower

filter' :: [TokenTag] -> [TokenTag]
filter' xs = filter isNgrams xs
    where
      isNgrams (TokenTag { .. }) = isJust _my_token_pos || isJust _my_token_ner

------------------------------------------------------------------------
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

corenlp' :: ( FromJSON a
            , ConvertibleStrings p ByteString
            )
          => URI -> Lang -> p -> IO (Response a)
corenlp' uri lang txt = do
  req <- parseRequest $
         "POST " <> show (uri { uriQuery = "?properties=" <> (BSL.unpack $ encode $ toJSON $ Map.fromList properties) })
   -- curl -XPOST 'http://localhost:9000/?properties=%7B%22annotators%22:%20%22tokenize,ssplit,pos,ner%22,%20%22outputFormat%22:%20%22json%22%7D' -d 'hello world, hello' | jq .
  httpJSON $ setRequestBodyLBS (cs txt) req
  where
    properties_ :: [(Text, Text)]
    properties_ = case lang of
-- TODO: Add: Aeson.encode $ Aeson.toJSON $ Map.fromList [()] instead of these hardcoded JSON strings
            EN -> [ ("annotators", "tokenize,ssplit,pos,ner" ) ]
            FR -> [ ("annotators", "tokenize,ssplit,pos,lemma,ner")
                  -- , ("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
                  , ("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
                  , ("tokenize.language", "fr") ]
            DE -> [ ("annotators", "tokenize,ssplit,pos,lemma,ner")
                  -- , ("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
                  , ("pos.model", "edu/stanford/nlp/models/pos-tagger/french/german-hgc.tagger")
                  , ("tokenize.language", "de") ]
            ES -> [ ("annotators", "tokenize,ssplit,pos,lemma,ner")
                  -- , ("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
                  , ("pos.model", "edu/stanford/nlp/models/pos-tagger/french/spanish.tagger")
                  , ("tokenize.language", "es") ]
            IT -> [ ("annotators", "tokenize,ssplit,pos,lemma,ner")
                  -- , ("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
                  -- , ("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
                  , ("tokenize.language", "it") ]
            PL -> [ ("annotators", "tokenize,ssplit,pos,lemma,ner")
                  -- , ("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
                  -- , ("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
                  , ("tokenize.language", "pl") ]
            CN -> [ ("annotators", "tokenize,ssplit,pos,lemma,ner")
                  -- , ("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
                  , ("pos.model", "edu/stanford/nlp/models/pos-tagger/french/chinese-distsim.tagger")
                  , ("tokenize.language", "zh") ]
            l  -> panic $ pack $ "corenlp for language " <> show l <> " is not implemented yet"

    properties = properties_ <> [ ("outputFormat", "json") ]



corenlp :: URI -> Lang -> Text -> IO PosSentences
corenlp uri lang txt = do
  response <- corenlp' uri lang txt
  pure (getResponseBody response)

-- | parseWith
-- Part Of Speech example
-- parseWith  _tokenPos    "Hello world."
-- == [[("``","``"),("Hello","UH"),("world","NN"),(".","."),("''","''")]]

-- Named Entity Recognition example
-- parseWith  _tokenNer     "Hello world of Peter."
-- [[("``","O"),("Hello","O"),("world","O"),("of","O"),("Peter","PERSON"),(".","O"),("''","O")]]
tokenWith :: URI -> (Token -> t) -> Lang -> Text -> IO [[(Text, t)]]
tokenWith uri f lang s = map (map (\t -> (_tokenWord t, f t)))
                  <$> map _sentenceTokens
                  <$> _sentences
                  <$> corenlp uri lang s

----------------------------------------------------------------------------------
-- Here connect to the JohnSnow Server as it has been done above with the corenlp'
-- We need the PosTagging according to the language and the lems
serverNLP :: Lang -> Text -> IO PosSentences
serverNLP = undefined
