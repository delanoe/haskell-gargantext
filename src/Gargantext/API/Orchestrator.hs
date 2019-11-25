{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Orchestrator where

import Gargantext.Prelude
import Gargantext.API.Settings
import Gargantext.API.Orchestrator.Types
import Gargantext.API.Orchestrator.Scrapy.Schedule
import Control.Lens hiding (elements)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Servant
import Servant.Job.Async
import Servant.Job.Client
import Servant.Job.Server
import Servant.Job.Utils (extendBaseUrl)

callJobScrapy :: (ToJSON e, FromJSON e, FromJSON o, MonadClientJob m)
              => JobServerURL e Schedule o
              -> (URL -> Schedule)
              -> m o
callJobScrapy jurl schedule = do
  progress $ NewTask jurl
  out <- view job_output <$>
          retryOnTransientFailure (clientCallbackJob' jurl
            (fmap (const ()) . scrapySchedule . schedule))
  progress $ Finished jurl Nothing
  pure out

logConsole :: ToJSON a => a -> IO ()
logConsole = LBS.putStrLn . encode

callScraper :: MonadClientJob m => URL -> ScraperInput -> m ScraperStatus
callScraper url input =
  callJobScrapy jurl $ \cb ->
    Schedule
      { s_project = "gargantext"
      , s_spider  = input ^. scin_spider
      , s_setting = []
      , s_jobid   = Nothing
      , s_version = Nothing
      , s_extra   =
          [("query",        input ^.. scin_query . _Just)
          ,("user",         [input ^. scin_user])
          ,("corpus",       [input ^. scin_corpus . to toUrlPiece])
          ,("report_every", input ^.. scin_report_every . _Just . to toUrlPiece)
          ,("limit",        input ^.. scin_limit . _Just . to toUrlPiece)
          ,("url",          input ^.. scin_local_file . _Just)
          ,("count_only",   input ^.. scin_count_only . _Just . to toUrlPiece)
          ,("callback",     [toUrlPiece cb])]
      }
  where
    jurl :: JobServerURL ScraperStatus Schedule ScraperStatus
    jurl = JobServerURL url Callback

pipeline :: FromJSON e => URL -> ClientEnv -> ScraperInput
                       -> (e -> IO ()) -> IO ScraperStatus
pipeline scrapyurl client_env input log_status = do
  e <- runJobMLog client_env log_status $ callScraper scrapyurl input
  either (panic . cs . show) pure e -- TODO throwError

scrapyOrchestrator :: Env -> IO (Server (WithCallbacks ScraperAPI))
scrapyOrchestrator env = do
  apiWithCallbacksServer (Proxy :: Proxy ScraperAPI)
    defaultSettings (extendBaseUrl ("scraper" :: String) $ env ^. env_self_url)
    (env ^. env_manager) (LogEvent logConsole) $
    serveJobsAPI (env ^. env_scrapers) .
      JobFunction . pipeline (URL $ env ^. env_settings . scrapydUrl)