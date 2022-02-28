{-# LANGUAGE TupleSections #-}
module Tracking
  ( tracking
  , ppTracked
  , EkgMetric
  , Step
  ) where

import Core
import Options
import Prelude

import Control.Monad.IO.Class
import Data.List (intersperse)
import Data.Text (Text)
import Servant.Client
import System.Metrics.Json (Value)

import Gargantext.API.Client

import qualified Data.Text as T

-- | e.g @["rts", "gc", "bytes_allocated"]@
type EkgMetric = [Text]
-- | Any textual description of a step
type Step      = Text

-- | Track EKG metrics before/after running a bunch of computations
--   that can talk to the backend.
tracking
  :: ClientOpts
  -> [Text] -- ^ e.g @["rts.gc.bytes_allocated"]@
  -> [(Step, ClientM a)]
  -> ClientM [Either [(EkgMetric, Value)] (Step, a)]
-- no steps, nothing to do
tracking _    _   []    = return []
-- no metrics to track, we just run the steps
tracking _    []  steps = traverse runStep steps
-- metrics to track: we intersperse metric fetching and steps,
-- starting and ending with metric fetching
tracking opts ms' steps = mix (Left <$> fetchMetrics) (map runStep steps)

  where fetchMetrics :: ClientM [(EkgMetric, Value)]
        fetchMetrics = flip traverse ms $ \metric -> do
          whenVerbose opts $
            liftIO . putStrLn $ "[Debug] metric to track: " ++ T.unpack (T.intercalate "." metric)
          dat <- (metric,) <$> getMetricSample metric
          whenVerbose opts $
            liftIO . putStrLn $ "[Debug] metric pulled: " ++ show dat
          return dat
        mix :: ClientM a -> [ClientM a] -> ClientM [a]
        mix x xs = sequence $ [x] ++ intersperse x xs ++ [x]
        ms = map (T.splitOn ".") ms'

-- ^ A trivial function to print results of steps and sampled metrics
ppTracked :: Show a => [Either [(EkgMetric, Value)] (Step, a)] -> ClientM ()
ppTracked [] = return ()
ppTracked (Right (step, a) : rest) = do
  liftIO . putStrLn $ "[step: " ++ T.unpack step ++ "] returned: " ++ show a
  ppTracked rest
ppTracked (Left ms : rest) = do
  liftIO . putStrLn $ unlines
    [ T.unpack (T.intercalate "." metric) ++ " = " ++ show val
    | (metric, val) <- ms
    ]
  ppTracked rest

runStep :: (Step, ClientM a) -> ClientM (Either e (Step, a))
runStep (step, act) = Right . (step,) <$> act
