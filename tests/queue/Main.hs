{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Either
import Data.List
import Prelude
import Test.Hspec

import Gargantext.Utils.Jobs
import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Monad
import Gargantext.Utils.Jobs.Queue (applyPrios, defaultPrios)
import Gargantext.Utils.Jobs.Settings
import Gargantext.Utils.Jobs.State

data JobT = A | B deriving (Eq, Ord, Show, Enum, Bounded)

data Counts = Counts { countAs :: Int, countBs :: Int }
  deriving (Eq, Show)

inc, dec :: JobT -> Counts -> Counts
inc A cs = cs { countAs = countAs cs + 1 }
inc B cs = cs { countBs = countBs cs + 1 }
dec A cs = cs { countAs = countAs cs - 1 }
dec B cs = cs { countBs = countBs cs - 1 }

testMaxRunners = do
  -- max runners = 2 with default settings
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  runningJs <- newTVarIO []
  let j num _inp l = do
        atomically $ modifyTVar runningJs (\xs -> ("Job #" ++ show num) : xs)
        -- putStrLn $ "Job #" ++ show num ++ " started"
        threadDelay (5 * 1000000) -- 5s
        -- putStrLn $ "Job #" ++ show num ++ " done"
        atomically $ modifyTVar runningJs (\xs -> filter (/=("Job #" ++ show num)) xs)
      jobs = [ (n, j n) | n <- [1..4] ]
  jids <- forM jobs $ \(i, f) -> do
    -- putStrLn ("Submitting job #" ++ show i)
    pushJob A () f settings st
  threadDelay 10000 -- 10ms
  r1 <- readTVarIO runningJs
  -- putStrLn ("Jobs running: " ++ show r1)
  sort r1 `shouldBe` ["Job #1", "Job #2"]
  threadDelay (6 * 1000000) -- 6s
  r2 <- readTVarIO runningJs
  sort r2 `shouldBe` ["Job #3", "Job #4"]
  threadDelay (5 * 1000000) -- 5s
  r3 <- readTVarIO runningJs
  r3 `shouldBe` []

testPrios = do
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings $
    applyPrios [(B, 10)] defaultPrios -- B has higher priority
  runningJs <- newTVarIO (Counts 0 0)
  let j num jobt _inp l = do
        atomically $ modifyTVar runningJs (inc jobt)
        -- putStrLn $ "Job #" ++ show num ++ " started"
        threadDelay (5 * 1000000) -- 5s
        -- putStrLn $ "Job #" ++ show num ++ " done"
        atomically $ modifyTVar runningJs (dec jobt)
      jobs = [ (0, A, j 0 A)
             , (1, A, j 1 A)
             , (2, B, j 2 B)
             , (3, B, j 3 B)
             ]
  jids <- forM jobs $ \(i, t, f) -> do
    -- putStrLn ("Submitting job #" ++ show i)
    pushJob t () f settings st
  threadDelay 10000 -- 10ms
  r1 <- readTVarIO runningJs
  r1 `shouldBe` (Counts 0 2)
  threadDelay (6 * 1000000) -- 6s
  r2 <- readTVarIO runningJs
  r2 `shouldBe` (Counts 2 0)
  threadDelay (5 * 1000000) -- 5s
  r3 <- readTVarIO runningJs
  r3 `shouldBe` (Counts 0 0)

testExceptions = do
  -- max runners = 2 with default settings
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  jid <- pushJob A ()
    (\_inp _log -> readFile "/doesntexist.txt" >>= putStrLn)
    settings st
  threadDelay 50000
  mjob <- lookupJob jid (jobsData st)
  case mjob of
    Nothing ->  error "boo"
    Just je ->  case jTask je of
      DoneJ _ r -> isLeft r `shouldBe` True
      _         -> error "boo2"
  return ()


main :: IO ()
main = hspec $ do
  describe "job queue" $ do
    it "respects max runners limit" $
      testMaxRunners
    it "respects priorities" $
      testPrios
    it "can handle exceptions" $
      testExceptions
