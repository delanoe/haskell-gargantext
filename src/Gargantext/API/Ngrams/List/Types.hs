module Gargantext.API.Ngrams.List.Types where

--{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}


--import Control.Lens hiding (elements, Indexed)
import Data.Aeson
import Data.Swagger (ToSchema, declareNamedSchema, genericDeclareNamedSchema)
import Data.Text
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded (FromForm, ToForm)

import Protolude

import Gargantext.API.Ngrams.Types (NgramsList)
import Gargantext.API.Node.Corpus.New.File (FileType(..))
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)

------------------------------------------------------------------------

data WithFile = WithFile
  { _wf_filetype :: !FileType
  , _wf_data     :: !NgramsList
  , _wf_name     :: !Text
  } deriving (Eq, Show, Generic)

--makeLenses ''WithFile
instance FromForm WithFile
instance ToForm WithFile
instance FromJSON WithFile where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToJSON WithFile where
  toJSON = genericToJSON $ jsonOptions "_wf_"
instance ToSchema WithFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")


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

