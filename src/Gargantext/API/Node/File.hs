{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-matches #-}

{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE IncoherentInstances #-}
module Gargantext.API.Node.File where

import Control.Lens ((^.))
import Data.Swagger
import Data.Text
import GHC.Generics (Generic)
import Servant
import Servant.Job.Async (JobFunction(..), serveJobsAPI)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.MIME.Types as DMT
import qualified Gargantext.Database.GargDB as GargDB
import qualified Network.HTTP.Media as M

import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Node.Types
import Gargantext.API.Prelude
import Gargantext.Core.Types (TODO)
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Node (mkNodeWithParent)
import Gargantext.Database.Admin.Types.Hyperdata.File
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node (getNodeWith)
import Gargantext.Database.Query.Table.Node.UpdateOpaleye (updateHyperdata)
import Gargantext.Database.Schema.Node (node_hyperdata)
import Gargantext.Prelude
import Data.Either

data RESPONSE deriving Typeable

instance Accept RESPONSE where
  contentType _ = "text" M.// "*"

instance MimeRender RESPONSE BSResponse where
  mimeRender _ (BSResponse val) = BSL.fromStrict $ val

type FileApi = Summary "File download"
            :> "download"
            :> Get '[RESPONSE] (Headers '[Servant.Header "Content-Type" Text] BSResponse)

instance MimeUnrender RESPONSE BSResponse where
  mimeUnrender _ lbs = Right $ BSResponse (BSL.toStrict lbs)

fileApi :: UserId -> NodeId -> GargServer FileApi
fileApi uId nId = fileDownload uId nId

newtype Contents = Contents BS.ByteString

instance GargDB.ReadFile Contents where
  readFile' fp = do
    c <- BS.readFile fp
    pure $ Contents c

newtype BSResponse = BSResponse BS.ByteString
  deriving (Generic)

instance ToSchema BSResponse  where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)

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

  Contents c <- GargDB.readFile $ unpack path

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

type FileAsyncApi = Summary "File Async Api"
                 :> "file"
                 :> "add"
                 :> AsyncJobs JobLog '[FormUrlEncoded] NewWithFile JobLog

fileAsyncApi :: UserId -> NodeId -> GargServer FileAsyncApi
fileAsyncApi uId nId =
  serveJobsAPI $
    JobFunction (\i l ->
      let
        log' x = do
          printDebug "addWithFile" x
          liftBase $ l x
      in addWithFile uId nId i log')


addWithFile :: (HasSettings env, FlowCmdM env err m)
            => UserId
            -> NodeId
            -> NewWithFile
            -> (JobLog -> m ())
            -> m JobLog
addWithFile uId nId nwf@(NewWithFile _d _l fName) logStatus = do

  printDebug "[addWithFile] Uploading file: " nId
  logStatus JobLog { _scst_succeeded = Just 0
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 1
                   , _scst_events    = Just []
                   }

  fPath <- GargDB.writeFile nwf
  printDebug "[addWithFile] File saved as: " fPath

  nIds <- mkNodeWithParent NodeFile (Just nId) uId fName

  _ <- case nIds of
    [nId'] -> do
        node <- getNodeWith nId' (Proxy :: Proxy HyperdataFile)
        let hl = node ^. node_hyperdata
        _ <- updateHyperdata nId' $ hl { _hff_name = fName
                                       , _hff_path = pack fPath }

        printDebug "[addWithFile] Created node with id: " nId'
    _     -> pure ()

  printDebug "[addWithFile] File upload finished: " nId
  pure $ JobLog { _scst_succeeded = Just 1
                , _scst_failed    = Just 0
                , _scst_remaining = Just 0
                , _scst_events    = Just []
                }
