module Gargantext.API.HashedResponse where

import Data.Aeson
import Data.Swagger
import Data.Text (Text)

import Gargantext.Prelude
import qualified Gargantext.Prelude.Utils as Crypto (hash)
import GHC.Generics (Generic)

data HashedResponse a = HashedResponse { hash :: Text, value :: a }
  deriving (Generic)

instance ToSchema a => ToSchema (HashedResponse a)
instance ToJSON a => ToJSON (HashedResponse a) where
  toJSON = genericToJSON defaultOptions

constructHashedResponse :: ToJSON a => a -> HashedResponse a
constructHashedResponse v = HashedResponse { hash = Crypto.hash $ encode v, value = v }
