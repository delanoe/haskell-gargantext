{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Gargantext.Utils.Jobs.Internal (
    serveJobsAPI
  , JobHandle -- opaque
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Foldable (toList)
import Data.Monoid
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prelude
import Servant.API

import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Monad

import qualified Data.Text as T
import qualified Servant.Client as C
import qualified Servant.Job.Async as SJ
import qualified Servant.Job.Client as SJ
import qualified Servant.Job.Types as SJ

-- | An opaque handle that abstracts over the concrete identifier for
-- a job. The constructor for this type is deliberately not exported.
newtype JobHandle =
  JobHandle { _jh_id :: SJ.JobID 'SJ.Safe }
  deriving (Eq, Ord)

serveJobsAPI
  :: ( Ord t, Exception e, MonadError e m
     , MonadJob m t (Seq event) output
     , ToJSON e, ToJSON event, ToJSON output
     , Foldable callback
     )
  => m env
  -> t
  -> (JobError -> e)
  -> (env -> JobHandle -> input -> Logger event -> IO (Either e output))
  -> SJ.AsyncJobsServerT' ctI ctO callback event input output m
serveJobsAPI getenv t joberr f
     = newJob getenv t f (SJ.JobInput undefined Nothing)
  :<|> newJob getenv t f
  :<|> serveJobAPI t joberr

serveJobAPI
  :: forall (m :: Type -> Type) e t event output.
     (Ord t, MonadError e m, MonadJob m t (Seq event) output)
  => t
  -> (JobError -> e)
  -> SJ.JobID 'SJ.Unsafe
  -> SJ.AsyncJobServerT event output m
serveJobAPI t joberr jid' = wrap' (killJob t)
                       :<|> wrap' pollJob
                       :<|> wrap (waitJob joberr)

  where wrap
          :: forall a.
             (SJ.JobID 'SJ.Safe -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output -> m a)
          -> m a
        wrap g = do
          jid <- handleIDError joberr (checkJID jid')
          job <- maybe (throwError $ joberr UnknownJob) pure =<< findJob jid
          g jid job

        wrap' g limit offset = wrap (g limit offset)

newJob
  :: ( Ord t, Exception e, MonadJob m t (Seq event) output
     , ToJSON e, ToJSON event, ToJSON output
     , Foldable callbacks
     )
  => m env
  -> t
  -> (env -> JobHandle -> input -> Logger event -> IO (Either e output))
  -> SJ.JobInput callbacks input
  -> m (SJ.JobStatus 'SJ.Safe event)
newJob getenv jobkind f input = do
  je <- getJobEnv
  env <- getenv
  let postCallback m = forM_ (input ^. SJ.job_callback) $ \url ->
        C.runClientM (SJ.clientMCallback m)
                     (C.mkClientEnv (jeManager je) (url  ^. SJ.base_url))

      pushLog logF e = do
        postCallback (SJ.mkChanEvent e)
        logF e

      f' jId inp logF = do
        r <- f env (JobHandle jId) inp (pushLog logF . Seq.singleton)
        case r of
          Left e  -> postCallback (SJ.mkChanError e) >> throwIO e
          Right a -> postCallback (SJ.mkChanResult a) >> return a

  jid <- queueJob jobkind (input ^. SJ.job_input) f'
  return (SJ.JobStatus jid [] SJ.IsPending Nothing)

pollJob
  :: MonadJob m t (Seq event) output
  => Maybe SJ.Limit
  -> Maybe SJ.Offset
  -> SJ.JobID 'SJ.Safe
  -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output
  -> m (SJ.JobStatus 'SJ.Safe event)
pollJob limit offset jid je = do
  (logs, status, merr) <- case jTask je of
    QueuedJ _    -> pure (mempty, SJ.IsPending, Nothing)
    RunningJ rj  -> (,,) <$> liftIO (rjGetLog rj)
                         <*> pure SJ.IsRunning
                         <*> pure Nothing
    DoneJ ls r ->
      let st = either (const SJ.IsFailure) (const SJ.IsFinished) r
          me = either (Just . T.pack . show) (const Nothing) r
      in pure (ls, st, me)
  pure $ SJ.jobStatus jid limit offset (toList logs) status merr

waitJob
  :: (MonadError e m, MonadJob m t (Seq event) output)
  => (JobError -> e)
  -> SJ.JobID 'SJ.Safe
  -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output
  -> m (SJ.JobOutput output)
waitJob joberr jid je = do
  r <- case jTask je of
    QueuedJ _qj -> do
      m <- getJobsMap
      erj <- waitTilRunning
      case erj of
        Left res -> return res
        Right rj -> do
          (res, _logs) <- liftIO (waitJobDone jid rj m)
          return res
    RunningJ rj -> do
      m <- getJobsMap
      (res, _logs) <- liftIO (waitJobDone jid rj m)
      return res
    DoneJ _ls res -> return res
  either (throwError . joberr . JobException) (pure . SJ.JobOutput) r

  where waitTilRunning =
          findJob jid >>= \mjob -> case mjob of
            Nothing -> error "impossible"
            Just je' -> case jTask je' of
              QueuedJ _qj -> do
                liftIO $ threadDelay 50000 -- wait 50ms
                waitTilRunning
              RunningJ rj -> return (Right rj)
              DoneJ _ls res -> return (Left res)

killJob
  :: (Ord t, MonadJob m t (Seq event) output)
  => t
  -> Maybe SJ.Limit
  -> Maybe SJ.Offset
  -> SJ.JobID 'SJ.Safe
  -> JobEntry (SJ.JobID 'SJ.Safe) (Seq event) output
  -> m (SJ.JobStatus 'SJ.Safe event)
killJob t limit offset jid je = do
  (logs, status, merr) <- case jTask je of
    QueuedJ _ -> do
      removeJob True t jid
      return (mempty, SJ.IsKilled, Nothing)
    RunningJ rj -> do
      liftIO $ cancel (rjAsync rj)
      lgs <- liftIO (rjGetLog rj)
      removeJob False t jid
      return (lgs, SJ.IsKilled, Nothing)
    DoneJ lgs r -> do
      let st = either (const SJ.IsFailure) (const SJ.IsFinished) r
          me = either (Just . T.pack . show) (const Nothing) r
      removeJob False t jid
      pure (lgs, st, me)
  pure $ SJ.jobStatus jid limit offset (toList logs) status merr
