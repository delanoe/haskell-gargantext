{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gargantext.API.EKG where

import Data.HashMap.Strict as HM
import Data.Text as T
import Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai
import Protolude
import Servant
import Servant.Auth
import Servant.Ekg
import System.Metrics
import qualified System.Metrics.Json as J

-- Mimics https://github.com/tibbe/ekg/blob/master/System/Remote/Snap.hs#L98
type EkgAPI =
  "ekg" :>
    ( "api" :>
       ( Get '[JSON] J.Sample :<|>
         CaptureAll "segments" Text :> Get '[JSON] J.Value
       ) :<|>
       Raw
    )

ekgServer :: FilePath -> Store -> Server EkgAPI
ekgServer assetsDir store = (getAll :<|> getOne) :<|> serveDirectoryFileServer assetsDir

  where getAll = J.Sample <$> liftIO (sampleAll store)
        getOne segments = do
          let metric = T.intercalate "." segments
          metrics <- liftIO (sampleAll store)
          maybe (liftIO (T.putStrLn "not found boohoo") >> throwError err404) (return . J.Value) (HM.lookup metric metrics)

newEkgStore :: HasEndpoint api => Proxy api -> IO (Store, Middleware)
newEkgStore api = do
  s <- newStore
  registerGcMetrics s
  registerCounter "ekg.server_timestamp_ms" getTimeMs s -- used by UI
  mid <- monitorEndpoints api s
  return (s, mid)

  where getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

instance HasEndpoint api => HasEndpoint (Auth xs a :> api) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy api)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy api)
