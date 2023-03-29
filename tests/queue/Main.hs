{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NumericUnderscores  #-}
module Main where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Either
import Data.List
import Data.Sequence (Seq)
import GHC.Generics
import GHC.Stack
import Prelude
import System.IO.Unsafe
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Client (Manager)
import Test.Hspec
import qualified Servant.Job.Types as SJ
import qualified Servant.Job.Core  as SJ

import Gargantext.Utils.Jobs.Internal (newJob)
import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Monad hiding (withJob)
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
jobDuration = 100000
initialDelay = 20000

testMaxRunners :: IO ()
testMaxRunners = do
  -- max runners = 2 with default settings
  k <- genSecret
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  runningJs <- newTVarIO []
  let j num _jHandle _inp _l = do
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
  let j jobt _jHandle _inp _l = do
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
  threadDelay (2*initialDelay)
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
    (\_jHandle _inp _log -> readFile "/doesntexist.txt" >>= putStrLn)
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
  let settings = defaultJobSettings 2 k
  st :: JobsState JobT [String] () <- newJobsState settings defaultPrios
  runningJs <- newTVarIO (Counts 0 0)
  let j jobt _jHandle _inp _l = do
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

data MyDummyJob
  = MyDummyJob
  deriving (Show, Eq, Ord, Enum, Bounded)

data MyDummyError
  = SomethingWentWrong JobError
  deriving (Show)

instance Exception MyDummyError where
  toException _ = toException (userError "SomethingWentWrong")

instance ToJSON MyDummyError where
  toJSON (SomethingWentWrong _) = String "SomethingWentWrong"

type Progress = Int

data MyDummyLog =
    Step_0 !Progress
  | Step_1 !Progress
  deriving (Show, Eq, Ord, Generic)

instance Monoid MyDummyLog where
  mempty = Step_0 0

instance Semigroup MyDummyLog where
  _ <> _ = error "not needed"

instance ToJSON MyDummyLog

newtype MyDummyEnv = MyDummyEnv { _MyDummyEnv :: JobEnv MyDummyJob (Seq MyDummyLog) () }

newtype MyDummyMonad a =
  MyDummyMonad { _MyDummyMonad :: ReaderT MyDummyEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader MyDummyEnv)

runMyDummyMonad :: MyDummyEnv -> MyDummyMonad a -> IO a
runMyDummyMonad env = flip runReaderT env . _MyDummyMonad

instance MonadJob MyDummyMonad MyDummyJob (Seq MyDummyLog) () where
  getJobEnv = asks _MyDummyEnv

instance MonadJobStatus MyDummyMonad where
  type JobType        MyDummyMonad = MyDummyJob
  type JobOutputType  MyDummyMonad = ()
  type JobEventType   MyDummyMonad = MyDummyLog

testTlsManager :: Manager
testTlsManager = unsafePerformIO newTlsManager
{-# NOINLINE testTlsManager #-}

shouldBeE :: (MonadIO m, HasCallStack, Show a, Eq a) => a -> a -> m ()
shouldBeE a b = liftIO (shouldBe a b)

type TheEnv = JobEnv MyDummyJob (Seq MyDummyLog) ()

withJob :: TheEnv
        -> (TheEnv -> JobHandle MyDummyMonad MyDummyLog -> () -> MyDummyMonad (Either MyDummyError ()))
        -> IO (SJ.JobStatus 'SJ.Safe MyDummyLog)
withJob myEnv f = runMyDummyMonad (MyDummyEnv myEnv) $
  newJob @_ @MyDummyError getJobEnv MyDummyJob (\env hdl input ->
    runMyDummyMonad (MyDummyEnv myEnv) $ f env hdl input) (SJ.JobInput () Nothing)

withJob_ :: TheEnv
         -> (TheEnv -> JobHandle MyDummyMonad MyDummyLog -> () -> MyDummyMonad (Either MyDummyError ()))
         -> IO ()
withJob_ env f = void (withJob env f)

testFetchJobStatus :: IO ()
testFetchJobStatus = do
  k <- genSecret
  let settings = defaultJobSettings 2 k
  myEnv <- newJobEnv settings defaultPrios testTlsManager
  evts <- newMVar []

  withJob_ myEnv $ \_ hdl _input -> do
    mb_status <- getLatestJobStatus hdl

    -- now let's log something
    updateJobProgress hdl (const $ Step_0 20)
    mb_status' <- getLatestJobStatus hdl
    updateJobProgress hdl (\(Step_0 x) -> Step_0 (x + 5))
    mb_status'' <- getLatestJobStatus hdl

    liftIO $ modifyMVar_ evts (\xs -> pure $ mb_status : mb_status' : mb_status'' : xs)
    pure $ Right ()

  threadDelay 500_000
  -- Check the events
  readMVar evts >>= \expected -> expected `shouldBe` [Nothing, Just (Step_0 20), Just (Step_0 25)]

testFetchJobStatusNoContention :: IO ()
testFetchJobStatusNoContention = do
  k <- genSecret
  let settings = defaultJobSettings 2 k
  myEnv <- newJobEnv settings defaultPrios testTlsManager

  evts1 <- newMVar []
  evts2 <- newMVar []

  let job1 = \() -> withJob_ myEnv $ \_ hdl _input -> do
        updateJobProgress hdl (const $ Step_1 100)
        mb_status <- getLatestJobStatus hdl
        liftIO $ modifyMVar_ evts1 (\xs -> pure $ mb_status : xs)
        pure $ Right ()

  let job2 = \() -> withJob_ myEnv $ \_ hdl _input -> do
        updateJobProgress hdl (const $ Step_0 50)
        mb_status <- getLatestJobStatus hdl
        liftIO $ modifyMVar_ evts2 (\xs -> pure $ mb_status : xs)
        pure $ Right ()

  Async.forConcurrently_ [job1, job2] ($ ())
  threadDelay 500_000
  -- Check the events
  readMVar evts1 >>= \expected -> expected `shouldBe` [Just (Step_1 100)]
  readMVar evts2 >>= \expected -> expected `shouldBe` [Just (Step_0 50)]

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
  describe "job status update and tracking" $ do
    it "can fetch the latest job status" $
      testFetchJobStatus
    it "can spin two separate jobs and track their status separately" $
      testFetchJobStatusNoContention
