{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}

module Gargantext.Utils.UTCTime where

import Data.Aeson (FromJSON, ToJSON)
import Data.Either (Either(..))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (GQLType(..), DecodeScalar(..), EncodeScalar(..))
import qualified Data.Morpheus.Types as DMT
import Data.Swagger (ToSchema)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Gargantext.Prelude
import GHC.Generics (Generic)
import Text.Read (readEither)


newtype NUTCTime = NUTCTime UTCTime
  deriving (Eq, Show, Generic)
instance DecodeScalar NUTCTime where
  decodeScalar (DMT.String x) = case (readEither $ T.unpack x) of
    Right r  -> pure $ NUTCTime r
    Left err -> Left $ T.pack err
  decodeScalar          _ = Left "Invalid value for NUTCTime"
instance EncodeScalar NUTCTime where
  encodeScalar (NUTCTime x) = DMT.String $ T.pack $ show x
instance GQLType NUTCTime where
  type KIND NUTCTime = SCALAR
instance FromJSON NUTCTime
instance ToJSON NUTCTime
instance ToSchema NUTCTime
