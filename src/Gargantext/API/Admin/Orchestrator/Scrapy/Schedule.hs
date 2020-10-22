{-|
Module      : Gargantext.API.Admin.Orchestartor.Scrapy.Schedule
Description : Server API Auth Module
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeOperators #-}

module Gargantext.API.Admin.Orchestrator.Scrapy.Schedule
  where

import Control.Lens
import Data.Aeson
import GHC.Generics
import Protolude
import Servant
import Servant.Client
import Servant.Job.Utils (jsonOptions)
import Web.FormUrlEncoded hiding (parseMaybe)
import qualified Data.HashMap.Strict as H

------------------------------------------------------------------------

data Schedule = Schedule
  { s_project :: !Text
  , s_spider  :: !Text
  , s_setting :: ![Text]
  , s_jobid   :: !(Maybe Text)
  , s_version :: !(Maybe Text)
  , s_extra   :: ![(Text,[Text])]
  }
  deriving (Generic)

data ScheduleResponse = ScheduleResponse
  { r_status :: !Text
  , r_jobid  :: !Text
  }
  deriving (Generic)

instance FromJSON ScheduleResponse where
  parseJSON = genericParseJSON (jsonOptions "r_")

instance ToForm Schedule where
  toForm s =
    Form . H.fromList $
      [("project",  [s_project s])
      ,("spider",   [s_spider  s])
      ,("setting",  s_setting s)
      ,("jobid",    s_jobid s ^.. _Just)
      ,("_version", s_version s ^.. _Just)
      ] ++ s_extra s

type Scrapy =
  "schedule.json" :> ReqBody '[FormUrlEncoded] Schedule
                  :> Post '[JSON] ScheduleResponse

scrapyAPI :: Proxy Scrapy
scrapyAPI = Proxy

scrapySchedule :: Schedule -> ClientM ScheduleResponse
scrapySchedule = client scrapyAPI
