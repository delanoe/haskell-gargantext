module Gargantext.API.HashedResponse where

import Data.Aeson
import Data.Swagger
import qualified Data.Digest.Pure.MD5 as DPMD5
import GHC.Generics (Generic)
import Protolude

data HashedResponse a = HashedResponse { md5 :: Text, value :: a }
  deriving (Generic)

instance ToSchema a => ToSchema (HashedResponse a)
instance ToJSON a => ToJSON (HashedResponse a) where
  toJSON = genericToJSON defaultOptions

constructHashedResponse :: ToJSON a => a -> HashedResponse a
constructHashedResponse v = HashedResponse { md5 = md5', value = v }
  where
    md5' = show $ DPMD5.md5 $ encode v