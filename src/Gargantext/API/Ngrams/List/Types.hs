{-|
Module      : Gargantext.API.Ngrams.List.Types
Description : Ngrams List Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.API.Ngrams.List.Types where

--{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}


--import Control.Lens hiding (elements, Indexed)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger (ToSchema, declareNamedSchema, genericDeclareNamedSchema)
import Data.Text
import qualified Data.Text.Encoding as E
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm(..), ToForm, parseUnique)

import Protolude

import Gargantext.API.Ngrams.Types (NgramsList)
import Gargantext.API.Node.Corpus.New.Types (FileType(..))
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)



data WithFile = WithFile
  { _wf_filetype :: !FileType
  , _wf_data     :: !NgramsList
  , _wf_name     :: !Text
  } deriving (Eq, Show, Generic)

--makeLenses ''WithFile
instance FromForm WithFile where
instance ToForm WithFile
instance FromJSON WithFile where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToJSON WithFile where
  toJSON = genericToJSON $ jsonOptions "_wf_"
instance ToSchema WithFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")

------------------------------------------------------------------------

data WithJsonFile = WithJsonFile
  { _wjf_data     :: !NgramsList
  , _wjf_name     :: !Text
  } deriving (Eq, Show, Generic)

instance FromForm WithJsonFile where
  fromForm f = do
    d' <- parseUnique "_wjf_data" f
    d <- case eitherDecode' (BSL.fromStrict $ E.encodeUtf8 d') of
      Left s -> Left $ pack s
      Right v -> Right v
    n <- parseUnique "_wjf_name" f
    pure $ WithJsonFile { _wjf_data = d
                        , _wjf_name = n }
instance ToForm WithJsonFile
instance FromJSON WithJsonFile where
  parseJSON = genericParseJSON $ jsonOptions "_wjf_"
instance ToJSON WithJsonFile where
  toJSON = genericToJSON $ jsonOptions "_wjf_"
instance ToSchema WithJsonFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wjf_")


------------------------------------------------------------------------

data WithTextFile = WithTextFile
  { _wtf_filetype :: !FileType
  , _wtf_data     :: !Text
  , _wtf_name     :: !Text
  } deriving (Eq, Show, Generic)

--makeLenses ''WithTextFile
instance FromForm WithTextFile
instance ToForm WithTextFile
instance FromJSON WithTextFile where
  parseJSON = genericParseJSON $ jsonOptions "_wtf_"
instance ToJSON WithTextFile where
  toJSON = genericToJSON $ jsonOptions "_wtf_"
instance ToSchema WithTextFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wtf_")
