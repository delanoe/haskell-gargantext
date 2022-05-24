{-# OPTIONS_GHC -fprint-potential-instances #-}

module Gargantext.API.Types where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import Data.Typeable
import Network.HTTP.Media ((//), (/:))
import Prelude (($))
import qualified Prelude
import Servant
  ( Accept(..)
  , MimeRender(..)
  , MimeUnrender(..) )

data HTML deriving (Typeable)
instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]
instance MimeRender HTML BS8.ByteString where
  mimeRender _ = Prelude.id
instance MimeUnrender HTML BS8.ByteString where
    mimeUnrender _ bs = Right bs
instance MimeRender HTML Text where
    mimeRender _ bs = BS8.fromStrict $ E.encodeUtf8 bs
instance MimeUnrender HTML Text where
    mimeUnrender _ bs = Right $ E.decodeUtf8 $ BS8.toStrict bs
instance {-# OVERLAPPABLE #-} ToJSON a => MimeRender HTML a where
  mimeRender _ = encode
