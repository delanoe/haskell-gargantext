module Gargantext.API.Job where

import Control.Lens (over, _Just)
import Data.Maybe
import qualified Data.Text as T

import Gargantext.Prelude

import Gargantext.API.Admin.Orchestrator.Types

newtype RemainingSteps = RemainingSteps { _RemainingSteps :: Int }
  deriving (Show, Eq, Num)

jobLogStart :: RemainingSteps -> JobLog
jobLogStart rem =
  JobLog { _scst_succeeded = Just 0
         , _scst_remaining = Just (_RemainingSteps rem)
         , _scst_failed = Just 0
         , _scst_events = Just [] }

addEvent :: T.Text -> T.Text -> JobLog -> JobLog
addEvent level message (JobLog { _scst_events = mEvts, .. }) = JobLog { _scst_events = Just (evts <> [ newEvt ]), .. }
  where
    evts = fromMaybe [] mEvts
    newEvt = ScraperEvent { _scev_message = Just message
                          , _scev_level = Just level
                          , _scev_date = Nothing }

addErrorEvent :: T.Text -> JobLog -> JobLog
addErrorEvent message = addEvent "ERROR" message

jobLogProgress :: Int -> JobLog -> JobLog
jobLogProgress n jl = over (scst_succeeded . _Just) (+ n) $
                      over (scst_remaining . _Just) (\x -> x - n) jl

-- | Mark a job as completely done, by adding the 'remaining' into 'succeeded'.
-- At the end 'scst_remaining' will be 0, and 'scst_succeeded' will be 'oldvalue + remaining'.
jobLogComplete :: JobLog -> JobLog
jobLogComplete jl =
  let remainingNow = fromMaybe 0 (_scst_remaining jl)
  in jl & over scst_succeeded (Just . maybe remainingNow ((+) remainingNow))
        & over scst_remaining (const (Just 0))

jobLogFailures :: Int -> JobLog -> JobLog
jobLogFailures n jl = over (scst_failed . _Just) (+ n) $
                over (scst_remaining . _Just) (\x -> x - n) jl

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
jobLogFailTotalWithMessage message jl = addErrorEvent message $ jobLogFailTotal jl

jobLogEvt :: JobLog -> ScraperEvent -> JobLog
jobLogEvt jl evt = over (scst_events . _Just) (\evts -> (evt:evts)) jl
