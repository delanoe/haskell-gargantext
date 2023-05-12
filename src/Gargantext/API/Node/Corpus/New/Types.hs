module Gargantext.API.Node.Corpus.New.Types where

import Data.Aeson
import Data.Swagger
import Data.Text (pack)
import GHC.Generics (Generic)
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Gargantext.Prelude

data FileType = CSV
              | CSV_HAL
              | PresseRIS
              | WOS
              | Iramuteq
              | JSON
  deriving (Eq, Show, Generic)
instance ToSchema FileType
instance Arbitrary FileType where arbitrary = elements [CSV, PresseRIS]
instance ToParamSchema FileType
instance FromJSON FileType
instance ToJSON FileType

instance FromHttpApiData FileType where
  parseUrlPiece "CSV"       = pure CSV
  parseUrlPiece "CSV_HAL"   = pure CSV_HAL
  parseUrlPiece "PresseRis" = pure PresseRIS
  parseUrlPiece "WOS"       = pure WOS
  parseUrlPiece "Iramuteq"  = pure Iramuteq
  parseUrlPiece "JSON"      = pure JSON
  parseUrlPiece s           = panic $ "[G.A.A.Node.Corpus.New] File Type not implemented (yet): " <> s
instance ToHttpApiData FileType where
  toUrlPiece = pack . show

data FileFormat = Plain | ZIP
  deriving (Eq, Show, Generic)
instance ToSchema FileFormat
instance Arbitrary FileFormat where arbitrary = elements [ Plain, ZIP ]
instance ToParamSchema FileFormat
instance FromJSON FileFormat
instance ToJSON FileFormat
instance FromHttpApiData FileFormat where
  parseUrlPiece "Plain" = pure Plain
  parseUrlPiece "ZIP"   = pure ZIP
  parseUrlPiece _       = pure Plain -- TODO error here
instance ToHttpApiData FileFormat where
  toUrlPiece = pack . show
