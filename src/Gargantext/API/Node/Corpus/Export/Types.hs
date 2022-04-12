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
import qualified Gargantext.API.Node.Document.Export.Types as DocumentExport
import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Servant


-- Corpus Export
data Corpus =
  Corpus { _c_corpus :: [DocumentExport.Document]
         , _c_hash   :: Hash
         } deriving (Generic)

type Hash = Text
-------
instance ToSchema Corpus where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_c_")
-------
instance ToParamSchema Corpus where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)
--------------------------------------------------
type API = Summary "Corpus Export"
            :> "export"
            :> QueryParam "listId"     ListId
            :> QueryParam "ngramsType" NgramsType
            :> Get '[JSON] Corpus

$(deriveJSON (unPrefix "_c_") ''Corpus)
