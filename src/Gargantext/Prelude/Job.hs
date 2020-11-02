module Gargantext.Prelude.Job where

import Data.IORef
import Data.Maybe

import Gargantext.Prelude

import Gargantext.API.Admin.Orchestrator.Types (JobLog(..))

jobLogInit :: Int -> JobLog
jobLogInit rem =
  JobLog { _scst_succeeded = Just 0
         , _scst_remaining = Just rem
         , _scst_failed = Just 0
         , _scst_events = Just [] }

jobLogSuccess :: JobLog -> JobLog
jobLogSuccess (JobLog { _scst_succeeded = mSucc
                      , _scst_remaining = mRem
                      , _scst_failed = mFail
                      , _scst_events = evt }) =
  JobLog { _scst_succeeded = (+ 1) <$> mSucc
         , _scst_remaining = (\x -> x - 1) <$> mRem
         , _scst_failed = mFail
         , _scst_events = evt }


jobLogFail :: JobLog -> JobLog
jobLogFail (JobLog { _scst_succeeded = mSucc
                   , _scst_remaining = mRem
                   , _scst_failed = mFail
                   , _scst_events = evt }) =
  JobLog { _scst_succeeded = mSucc
         , _scst_remaining = (\x -> x - 1) <$> mRem
         , _scst_failed = (+ 1) <$> mFail
         , _scst_events = evt }

runJobLog :: Int -> (JobLog -> IO ()) -> IO (IO (), IO (), IO JobLog)
runJobLog num logStatus = do
  jlRef <- newIORef $ jobLogInit num

  let logRef = do
    jl <- readIORef jlRef
    logStatus jl
  let logRefSuccess = do
    jl <- readIORef jlRef
    writeIORef $ jobLogSuccess jl
  let getRef = do
    readIORef jlRef

  return (logRef, logRefSuccess, getRef)
