module Gargantext.API.HashedResponse where

import Data.Aeson
import Data.Swagger
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Prelude.Utils (hash)
import GHC.Generics (Generic)

data HashedResponse a = HashedResponse { md5 :: Text, value :: a }
  deriving (Generic)

instance ToSchema a => ToSchema (HashedResponse a)
instance ToJSON a => ToJSON (HashedResponse a) where
  toJSON = genericToJSON defaultOptions

constructHashedResponse :: ToJSON a => a -> HashedResponse a
constructHashedResponse v = HashedResponse { md5 = md5', value = v }
  where
    md5' = hash $ encode v
