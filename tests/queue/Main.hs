{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Either
import Data.List
import Prelude
import Test.Hspec

import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Monad
import Gargantext.Utils.Jobs.Queue (applyPrios, defaultPrios)
import Gargantext.Utils.Jobs.State

data JobT = A | B deriving (Eq, Ord, Show, Enum, Bounded)

data Counts = Counts { countAs :: Int, countBs :: Int }
  deriving (Eq, Show)

inc, dec :: JobT -> Counts -> Counts
inc A cs = cs { countAs = countAs cs + 1 }
inc B cs = cs { countBs = countBs cs + 1 }
dec A cs = cs { countAs = countAs cs - 1 }
dec B cs = cs { countBs = countBs cs - 1 }

jobDuration, initialDelay :: Int
jobDuration = 100000 -- 100ms
initialDelay = 30000 -- 10ms

testMaxRunners :: IO ()
testMaxRunners = do
  -- max runners = 2 with default settings
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  runningJs <- newTVarIO []
  let j num _inp _l = do
        atomically $ modifyTVar runningJs (\xs -> ("Job #" ++ show num) : xs)
        threadDelay jobDuration
        atomically $ modifyTVar runningJs (\xs -> filter (/=("Job #" ++ show num)) xs)
      jobs = [ j n | n <- [1..4::Int] ]
  _jids <- forM jobs $ \f -> pushJob A () f settings st
  threadDelay initialDelay
  r1 <- readTVarIO runningJs
  sort r1 `shouldBe` ["Job #1", "Job #2"]
  threadDelay jobDuration
  r2 <- readTVarIO runningJs
  sort r2 `shouldBe` ["Job #3", "Job #4"]
  threadDelay jobDuration
  r3 <- readTVarIO runningJs
  r3 `shouldBe` []

testPrios :: IO ()
testPrios = do
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings $
    applyPrios [(B, 10)] defaultPrios -- B has higher priority
  runningJs <- newTVarIO (Counts 0 0)
  let j jobt _inp _l = do
        atomically $ modifyTVar runningJs (inc jobt)
        threadDelay jobDuration
        atomically $ modifyTVar runningJs (dec jobt)
      jobs = [ (A, j A)
             , (A, j A)
             , (B, j B)
             , (B, j B)
             ]
  _jids <- forM jobs $ \(t, f) -> do
    pushJob t () f settings st
  threadDelay initialDelay
  r1 <- readTVarIO runningJs
  r1 `shouldBe` (Counts 0 2)
  threadDelay jobDuration
  r2 <- readTVarIO runningJs
  r2 `shouldBe` (Counts 2 0)
  threadDelay jobDuration
  r3 <- readTVarIO runningJs
  r3 `shouldBe` (Counts 0 0)

testExceptions :: IO ()
testExceptions = do
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  jid <- pushJob A ()
    (\_inp _log -> readFile "/doesntexist.txt" >>= putStrLn)
    settings st
  threadDelay initialDelay
  mjob <- lookupJob jid (jobsData st)
  case mjob of
    Nothing ->  error "boo"
    Just je ->  case jTask je of
      DoneJ _ r -> isLeft r `shouldBe` True
      _         -> error "boo2"
  return ()

testFairness :: IO ()
testFairness = do
  k <- genSecret
  let settings = defaultJobSettings k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  runningJs <- newTVarIO (Counts 0 0)
  let j jobt _inp _l = do
        atomically $ modifyTVar runningJs (inc jobt)
        threadDelay jobDuration
        atomically $ modifyTVar runningJs (dec jobt)
      jobs = [ (A, j A)
             , (A, j A)
             , (B, j B)
             , (A, j A)
             , (A, j A)
             ]
  _jids <- forM jobs $ \(t, f) -> do
    pushJob t () f settings st
  threadDelay initialDelay
  r1 <- readTVarIO runningJs
  r1 `shouldBe` (Counts 2 0)
  threadDelay jobDuration
  r2 <- readTVarIO runningJs
  r2 `shouldBe` (Counts 1 1) -- MOST IMPORTANT CHECK: the B got picked after the
                             -- two As, because it was inserted right after them
                             -- and has equal priority.
  threadDelay jobDuration
  r3 <- readTVarIO runningJs
  r3 `shouldBe` (Counts 1 0)
  threadDelay jobDuration
  r4 <- readTVarIO runningJs
  r4 `shouldBe` (Counts 0 0)

main :: IO ()
main = hspec $ do
  describe "job queue" $ do
    it "respects max runners limit" $
      testMaxRunners
    it "respects priorities" $
      testPrios
    it "can handle exceptions" $
      testExceptions
    it "fairly picks equal-priority-but-different-kind jobs" $
      testFairness
