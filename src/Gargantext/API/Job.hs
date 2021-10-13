module Gargantext.API.Job where

import Control.Lens (over, _Just)
import Data.IORef
import Data.Maybe
import qualified Data.Text as T

import Gargantext.Prelude

import Gargantext.API.Admin.Orchestrator.Types


jobLogInit :: Int -> JobLog
jobLogInit rem =
  JobLog { _scst_succeeded = Just 0
         , _scst_remaining = Just rem
         , _scst_failed = Just 0
         , _scst_events = Just [] }

addEvent :: T.Text -> T.Text -> JobLog -> JobLog
addEvent level message (JobLog { _scst_events = mEvts, .. }) = JobLog { _scst_events = Just (evts <> [ newEvt ]), .. }
  where
    evts = fromMaybe [] mEvts
    newEvt = ScraperEvent { _scev_message = Just message
                          , _scev_level = Just level
                          , _scev_date = Nothing }

jobLogSuccess :: JobLog -> JobLog
jobLogSuccess jl = over (scst_succeeded . _Just) (+ 1) $
                   over (scst_remaining . _Just) (\x -> x - 1) jl

jobLogFail :: JobLog -> JobLog
jobLogFail jl = over (scst_failed . _Just) (+ 1) $
                over (scst_remaining . _Just) (\x -> x - 1) jl

jobLogFailTotal :: JobLog -> JobLog
jobLogFailTotal (JobLog { _scst_succeeded = mSucc
                        , _scst_remaining = mRem
                        , _scst_failed = mFail
                        , _scst_events = evt }) =
  JobLog { _scst_succeeded = mSucc
         , _scst_remaining = newRem
         , _scst_failed = newFail
         , _scst_events = evt }
  where
    (newRem, newFail) = case mRem of
      Nothing -> (Nothing, mFail)
      Just rem -> (Just 0, (+ rem) <$> mFail)

jobLogFailTotalWithMessage :: T.Text -> JobLog -> JobLog
jobLogFailTotalWithMessage message jl = addEvent "ERROR" message $ jobLogFailTotal jl

jobLogEvt :: JobLog -> ScraperEvent -> JobLog
jobLogEvt jl evt = over (scst_events . _Just) (\evts -> (evt:evts)) jl

runJobLog :: MonadBase IO m => Int -> (JobLog -> m ()) -> m (m (), m (), m JobLog)
runJobLog num logStatus = do
  jlRef <- liftBase $ newIORef $ jobLogInit num

  return (logRefF jlRef, logRefSuccessF jlRef, getRefF jlRef)

  where
    logRefF ref = do
      jl <- liftBase $ readIORef ref
      logStatus jl
    logRefSuccessF ref = do
      jl <- liftBase $ readIORef ref
      let jl' = jobLogSuccess jl
      liftBase $ writeIORef ref jl'
      logStatus jl'
    getRefF ref = do
      liftBase $ readIORef ref
