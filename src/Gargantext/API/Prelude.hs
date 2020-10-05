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
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Crypto.JOSE.Error as Jose
import Data.Aeson.Types
import Data.Typeable
import Data.Validity
import Servant
import Servant.Job.Async
import Servant.Job.Core (HasServerError(..), serverError)

import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Admin.Types
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (NodeError(..), HasNodeError(..))
import Gargantext.Database.Query.Tree
import Gargantext.Prelude

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
    , HasRepo      env  -- TODO rename HasNgramsRepo
    , HasSettings  env  -- TODO rename HasDbSettings
    , HasJobEnv    env JobLog JobLog
    , HasConfig    env
    )


type GargServer api =
  forall env err m. GargServerT env err m api

type GargServerT env err m api = GargServerC env err m => ServerT api m

-- This is the concrete monad. It needs to be used as little as possible,
-- instead, prefer GargServer, GargServerT, GargServerC.
type GargServerM env err = ReaderT env (ExceptT err IO)

type EnvC env =
  ( HasConnectionPool env
  , HasRepo           env
  , HasSettings       env
  , HasJobEnv         env JobLog JobLog
  , HasConfig         env
  )

-------------------------------------------------------------------
-- | This Type is needed to prepare the function before the GargServer
type GargNoServer t =
  forall env err m. GargNoServer' env err m => m t

type GargNoServer' env err m =
  ( CmdM           env err m
  , HasRepo        env
  , HasSettings    env
  , HasNodeError       err
  )

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
  _ <- mapM (\n -> simuTask logStatus n t) $ take t [0,1..]
  pure $ JobLog { _scst_succeeded = Just t
                , _scst_failed    = Just 0
                , _scst_remaining = Just 0
                , _scst_events    = Just []
                }

simuTask :: MonadBase IO m
          => (JobLog -> m ())
          -> Int
          -> Int
          -> m ()
simuTask logStatus cur total = do
  let m = (10 :: Int) ^ (6 :: Int)
  liftBase $ threadDelay (m*5)

  let status =  JobLog { _scst_succeeded = Just cur
                       , _scst_failed    = Just 0
                       , _scst_remaining = (-) <$> Just total <*> Just cur
                       , _scst_events    = Just []
                       }
  printDebug "status" status
  logStatus status
