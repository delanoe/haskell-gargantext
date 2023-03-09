{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.Utils.Jobs.Settings where

import Control.Lens
import Prelude

import qualified Servant.Job.Core as SJ

-- | A few control knobs for the job system.
data JobSettings = JobSettings
  { jsNumRunners :: Int
  , jsJobTimeout :: Int -- in seconds. TODO: timeout per job type? Map t Int
  , jsIDTimeout  :: Int -- in seconds, how long a job ID is valid
  , jsGcPeriod   :: Int -- in seconds, how long between each GC
  , jsSecretKey  :: SJ.SecretKey
  }

makeLensesFor [ ("jsJobTimeout", "l_jsJobTimeout")
              , ("jsIDTimeout", "l_jsIDTimeout")] ''JobSettings
