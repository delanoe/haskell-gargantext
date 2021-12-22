{-|
Module      : Gargantext.API.Node.Document.Export.Types
Description : Types for Gargantext.API.Node.Document.Export
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Gargantext.API.Node.Document.Export.Types where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Servant


-- | Document Export
data DocumentExport =
  DocumentExport { _de_documents    :: [Document]
                 , _de_garg_version :: Text
                 } deriving (Generic)

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
instance ToSchema DocumentExport where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_de_")

instance ToSchema Document where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_d_")

instance ToSchema Ngrams where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ng_")

-------
instance ToParamSchema DocumentExport where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Document where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance ToParamSchema Ngrams where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)
--------------------------------------------------
type API = Summary "Document Export"
            :> "export"
            :> Get '[JSON] DocumentExport

$(deriveJSON (unPrefix "_de_") ''DocumentExport)
$(deriveJSON (unPrefix "_d_") ''Document)
$(deriveJSON (unPrefix "_ng_") ''Ngrams)
