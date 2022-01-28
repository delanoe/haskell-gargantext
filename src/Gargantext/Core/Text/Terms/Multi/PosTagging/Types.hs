{-|
Module      : Gargantext.Core.Text.Terms.Multi.PosTagging.Types
Description : PosTagging module using Stanford java REST API
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.Core.Text.Terms.Multi.PosTagging.Types where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude
import GHC.Generics


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

data Sentence  = Sentence { _sentenceIndex  :: Int
                          , _sentenceTokens :: [Token]
                          } deriving (Show, Generic)

$(deriveJSON (unPrefix "_sentence") ''Sentence)

data Properties = Properties { _propertiesAnnotators  :: Text
                             , _propertiesOutputFormat :: Text
                             } deriving (Show, Generic)

$(deriveJSON (unPrefix "_properties") ''Properties)

data PosSentences = PosSentences { _sentences :: [Sentence]}
  deriving (Show, Generic)

$(deriveJSON (unPrefix "_") ''PosSentences)

