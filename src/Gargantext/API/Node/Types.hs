{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.API.Node.Types where

import Control.Lens hiding (elements, Empty)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BSB64
import Data.Either
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded          (FromForm, ToForm)

import Gargantext.Core (Lang(..){-, allLangs-})
import Gargantext.Core.Utils.Prefix (unPrefixSwagger)
import Gargantext.Prelude
import qualified Gargantext.Database.GargDB as GargDB
import Gargantext.API.Node.Corpus.New.Types (FileType, FileFormat)

-------------------------------------------------------
data NewWithForm = NewWithForm
  { _wf_filetype   :: !FileType
  , _wf_fileformat :: !FileFormat
  , _wf_data       :: !Text    -- NOTE for binary files, this represents base-64 data
  , _wf_lang       :: !(Maybe Lang)
  , _wf_name       :: !Text
  } deriving (Eq, Show, Generic)

makeLenses ''NewWithForm
instance FromForm NewWithForm
instance ToForm NewWithForm
instance FromJSON NewWithForm where
  parseJSON = genericParseJSON $ jsonOptions "_wf_"
instance ToJSON NewWithForm where
  toJSON = genericToJSON $ jsonOptions "_wf_"
instance ToSchema NewWithForm where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wf_")

-------------------------------------------------------

data NewWithFile = NewWithFile
  { _wfi_b64_data :: !Text
  , _wfi_lang     :: !(Maybe Lang)
  , _wfi_name     :: !Text
  } deriving (Eq, Show, Generic)

makeLenses ''NewWithFile
instance FromForm NewWithFile
instance ToForm NewWithFile
instance FromJSON NewWithFile where
  parseJSON = genericParseJSON $ jsonOptions "_wfi_"
instance ToJSON NewWithFile where
  toJSON = genericToJSON $ jsonOptions "_wfi_"


instance ToSchema NewWithFile where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_wfi_")

instance GargDB.SaveFile NewWithFile where
  saveFile' fp (NewWithFile b64d _ _) = do
    let eDecoded = BSB64.decode $ TE.encodeUtf8 b64d
    case eDecoded of
      Left err -> panic $ T.pack $ "Error decoding: " <> err
      Right decoded -> BS.writeFile fp decoded
    -- BS.writeFile fp $ BSB64.decodeLenient $ TE.encodeUtf8 b64d

--instance GargDB.ReadFile NewWithFile where
--  readFile' = TIO.readFile
