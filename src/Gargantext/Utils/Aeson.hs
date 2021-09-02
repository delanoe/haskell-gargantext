module Gargantext.Utils.Aeson where

import Data.Aeson.Types

-- this is what purescript Simple.JSON generics assumes
defaultTaggedObject :: SumEncoding
defaultTaggedObject = TaggedObject { tagFieldName = "type", contentsFieldName = "value" }
