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
import Data.Set (fromList)
import Data.Text (Text, splitOn, pack, toLower)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Terms.Multi.PosTagging.Types
import Gargantext.Core.Types
import Gargantext.Prelude
import Network.HTTP.Simple

------------------------------------------------------------------------
------------------------------------------------------------------------
tokens2tokensTags :: [Token] -> [TokenTag]
tokens2tokensTags ts = filter' $ map tokenTag ts
------------------------------------------------------------------------
tokenTag :: Token -> TokenTag
tokenTag (Token _ _ w l _ _ p n _ _) = TokenTag w' l' p n
  where
    w' = split w
    l' = fromList (split l)
    split = splitOn (pack " ") . toLower

filter' :: [TokenTag] -> [TokenTag]
filter' xs = filter isNgrams xs
    where
      isNgrams (TokenTag _ _ p n) = isJust p || isJust n

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
          => Lang -> p -> IO (Response a)
corenlp' lang txt = do
    let properties = case lang of
            EN -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}"
            -- FR -> "{\"annotators\": \"tokenize,ssplit,pos,ner\", \"outputFormat\": \"json\"}"
            FR -> "{\"annotators\": \"tokenize,ssplit,pos,lemma,ner\", \"parse.model\":\"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz\", \"pos.model\":\"edu/stanford/nlp/models/pos-tagger/french/french.tagger\", \"tokenize.language\":\"fr\", \"outputFormat\": \"json\"}"
            _  -> panic $ pack "not implemented yet"
    url <- parseRequest $ "POST http://localhost:9000/?properties=" <> properties
    let request = setRequestBodyLBS (cs txt) url
    httpJSON request

corenlpRaw :: Lang -> Text -> IO Value
corenlpRaw lang txt = do
  response <- corenlp' lang txt
  pure (getResponseBody response)


corenlp :: Lang -> Text -> IO PosSentences
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

----------------------------------------------------------------------------------
-- Here connect to the JohnSnow Server as it has been done above with the corenlp'
-- We need the PosTagging according to the language and the lems
serverNLP :: Lang -> Text -> IO PosSentences
serverNLP = undefined
