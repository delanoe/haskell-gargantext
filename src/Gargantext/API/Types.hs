{-# OPTIONS_GHC -fprint-potential-instances #-}

module Gargantext.API.Types where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Typeable
import Network.HTTP.Media ((//), (/:))
import qualified Prelude
import Servant
  ( Accept(..)
  , MimeRender(..) )

data HTML deriving (Typeable)
instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]
instance MimeRender HTML ByteString where
  mimeRender _ = Prelude.id
instance ToJSON a => MimeRender HTML a where
  mimeRender _ = encode
