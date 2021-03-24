{-|
Module      : Gargantext.API.Node.Corpus.Export.Types
Description : Types for Gargantext.API.Node.Corpus.Export
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Gargantext.API.Node.Corpus.Export.Types where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Servant


-- Corpus Export
data Corpus =
  Corpus { _c_corpus :: [Document]
         , _c_hash   :: Hash
         } deriving (Generic)

-- | Document Export
data Document =
  Document { _d_document :: Node HyperdataDocument
           , _d_ngrams   :: Ngrams
           , _d_hash     :: Hash
           } deriving (Generic)

data Ngrams =
  Ngrams { _ng_ngrams :: [Text]
         , _ng_hash   :: Hash
         } deriving (Generic)

type Hash = Text
-------
instance ToSchema Corpus where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_c_")

instance ToSchema Document where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_d_")

instance ToSchema Ngrams where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ng_")

-------
instance ToParamSchema Corpus where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Document where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Ngrams where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)
--------------------------------------------------
type API = Summary "Corpus Export"
            :> "export"
            :> QueryParam "listId"     ListId
            :> QueryParam "ngramsType" NgramsType
            :> Get '[JSON] Corpus

$(deriveJSON (unPrefix "_c_") ''Corpus)
$(deriveJSON (unPrefix "_d_") ''Document)
$(deriveJSON (unPrefix "_ng_") ''Ngrams)