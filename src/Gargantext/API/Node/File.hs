{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-matches -fno-warn-unused-imports #-}

module Gargantext.API.Node.File where

import Control.Lens ((^.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.MIME.Types as DMT
import Data.Monoid (mempty)
import Data.Swagger
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import qualified Network.HTTP.Media as M
import Network.Wai.Application.Static
import Servant
import Servant.API.Raw (Raw)
import Servant.Server.Internal

import Gargantext.Prelude
import qualified Gargantext.Prelude.Utils as GPU

import Gargantext.API.Admin.Settings (HasSettings)
import Gargantext.API.Prelude
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Admin.Types.Hyperdata.File
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getNodeWith)
import Gargantext.Database.Schema.Node (node_hyperdata)

data RESPONSE deriving Typeable

instance Accept RESPONSE where
  contentType _ = "text" M.// "*"

instance MimeRender RESPONSE BSResponse where
  mimeRender _ (BSResponse val) = BSL.fromStrict $ val

type FileApi = Summary "File download"
            :> "download"
            :> Get '[RESPONSE] (Headers '[Servant.Header "Content-Type" Text] BSResponse)

fileApi :: UserId -> NodeId -> GargServer FileApi
fileApi uId nId = fileDownload uId nId

newtype Contents = Contents BS.ByteString

instance GPU.ReadFile Contents where
  readFile' fp = do
    c <- BS.readFile fp
    pure $ Contents c

newtype BSResponse = BSResponse BS.ByteString
  deriving (Generic)

instance ToSchema BSResponse  where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy BSResponse)

fileDownload :: (HasSettings env, FlowCmdM env err m)
             => UserId
             -> NodeId
             -> m (Headers '[Servant.Header "Content-Type" Text] BSResponse)
fileDownload uId nId = do
  printDebug "[fileDownload] uId" uId
  printDebug "[fileDownload] nId" nId

  node <- getNodeWith nId (Proxy :: Proxy HyperdataFile)
  let (HyperdataFile { _hff_name = name'
                     , _hff_path = path }) = node ^. node_hyperdata

  Contents c <- GPU.readFile $ unpack path

  let (mMime, _) = DMT.guessType DMT.defaultmtd False $ unpack name'
      mime = case mMime of
        Just m  -> m
        Nothing -> "text/plain"

  pure $ addHeader (pack mime) $ BSResponse c
 
  --pure c

  -- let settings = embeddedSettings [("", encodeUtf8 c)]

  -- Tagged $ staticApp settings

  -- let settings = embeddedSettings [("", "hello")]
  -- Tagged $ staticApp settings

