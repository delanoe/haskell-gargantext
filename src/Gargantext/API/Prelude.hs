{-|
Module      : Gargantext.API.Prelude
Description : Server API main Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Gargantext.API.Prelude
  ( module Gargantext.API.Prelude
  , HasServerError(..)
  , serverError
  )
  where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Lens (Prism', (#))
import Control.Lens.TH (makePrisms)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Crypto.JOSE.Error as Jose
import Data.Aeson.Types
import Data.Typeable
import Data.Validity
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Admin.Settings
import Gargantext.API.Ngrams
import Gargantext.Core.Types
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (NodeError(..), HasNodeError(..))
import Gargantext.Database.Query.Tree
import Gargantext.Prelude
import Servant
import Servant.Job.Async (HasJobEnv)
import Servant.Job.Core (HasServerError(..), serverError)

class HasJoseError e where
  _JoseError :: Prism' e Jose.Error

joseError :: (MonadError e m, HasJoseError e) => Jose.Error -> m a
joseError = throwError . (_JoseError #)

class ThrowAll' e a | a -> e where
  -- | 'throwAll' is a convenience function to throw errors across an entire
  -- sub-API
  --
  --
  -- > throwAll err400 :: Handler a :<|> Handler b :<|> Handler c
  -- >    == throwError err400 :<|> throwError err400 :<|> err400
  throwAll' :: e -> a

instance (ThrowAll' e a, ThrowAll' e b) => ThrowAll' e (a :<|> b) where
  throwAll' e = throwAll' e :<|> throwAll' e

-- Really this shouldn't be necessary - ((->) a) should be an instance of
-- MonadError, no?
instance {-# OVERLAPPING #-} ThrowAll' e b => ThrowAll' e (a -> b) where
  throwAll' e = const $ throwAll' e

instance {-# OVERLAPPABLE #-} (MonadError e m) => ThrowAll' e (m a) where
  throwAll' = throwError

type GargServerC env err m =
    ( CmdM         env err m
    , HasNodeError     err
    , HasInvalidError  err
    , HasTreeError     err
    , HasServerError   err
    , HasJoseError     err
    , ToJSON           err -- TODO this is arguable
    , Exception        err
    , HasRepo      env
    , HasSettings  env
    , HasJobEnv    env JobLog JobLog
    )

type GargServerT env err m api = GargServerC env err m => ServerT api m

type GargServer api =
  forall env err m. GargServerT env err m api

-- This is the concrete monad. It needs to be used as little as possible,
-- instead, prefer GargServer, GargServerT, GargServerC.
type GargServerM env err = ReaderT env (ExceptT err IO)

type EnvC env =
  ( HasConnectionPool env
  , HasRepo env
  , HasSettings env
  , HasJobEnv env JobLog JobLog
  )

-------------------------------------------------------------------
runCmdReplEasy :: Cmd' DevEnv GargError a -> IO a
runCmdReplEasy f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f

-------------------------------------------------------------------
-- | This Type is needed to prepare the function before the GargServer
type GargNoServer' env err m =
  ( CmdM           env err m
  , HasRepo        env
  , HasSettings    env
  , HasNodeError       err
  )

type GargNoServer t =
  forall env err m. GargNoServer' env err m => m t
-------------------------------------------------------------------

data GargError
  = GargNodeError    NodeError
  | GargTreeError    TreeError
  | GargInvalidError Validation
  | GargJoseError    Jose.Error
  | GargServerError  ServerError
  deriving (Show, Typeable)

makePrisms ''GargError

instance ToJSON GargError where
  toJSON _ = String "SomeGargErrorPleaseReport"

instance Exception GargError

instance HasNodeError GargError where
  _NodeError = _GargNodeError

instance HasInvalidError GargError where
  _InvalidError = _GargInvalidError

instance HasTreeError GargError where
  _TreeError = _GargTreeError

instance HasServerError GargError where
  _ServerError = _GargServerError

instance HasJoseError GargError where
  _JoseError = _GargJoseError


------------------------------------------------------------------------
-- | Utils
-- | Simulate logs

simuLogs  :: MonadBase IO m
         => (JobLog -> m ())
         -> Int
         -> m JobLog
simuLogs logStatus t = do
{-
  let task = JobLog { _scst_succeeded = Just 0
                           , _scst_failed    = Just 0
                           , _scst_remaining = Just 0
                           , _scst_events    = Just []
                           }
-}
  -- f <- mapM (\status n -> simuTask logStatus status n t) task $ take t [1,2..]
  _ <- mapM (\n -> simuTask' logStatus n t) $ take t [1,2..]
  pure $ JobLog { _scst_succeeded = Just t
                        , _scst_failed    = Just 0
                        , _scst_remaining = Just 0
                        , _scst_events    = Just []
                        }


{-
simuTask :: MonadBase IO m
         => (JobLog -> m ())
         -> JobLog
         -> Int
         -> Int
         -> m JobLog
simuTask logStatus (JobLog _s f _r e) n t = do
  let
    m = (10 :: Int) ^ (6 :: Int)
  _ <- liftBase $ threadDelay ( m * 10)

  let status =  JobLog { _scst_succeeded = Just n
                              , _scst_failed    = f
                              , _scst_remaining = (-) <$> Just t <*> Just n
                              , _scst_events    = e
                              }
  printDebug "status" status
  logStatus status
  pure status
-}

simuTask' :: MonadBase IO m
          => (JobLog -> m ())
          -> Int
          -> Int
          -> m ()
simuTask' logStatus cur total = do
  let
    m = (10 :: Int) ^ (6 :: Int)
  _ <- liftBase $ threadDelay ( m * 10)

  let status =  JobLog { _scst_succeeded = Just cur
                              , _scst_failed    = Just 0
                              , _scst_remaining = (-) <$> Just total <*> Just cur
                              , _scst_events    = Just []
                              }
  printDebug "status" status
  logStatus status




