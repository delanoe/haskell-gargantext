{-|
Module      : Gargantext.API.Types
Description : Server API main Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Gargantext.API.Types
  ( module Gargantext.API.Types
  , HasServerError(..)
  , serverError
  )
  where

import Control.Exception (Exception)
import Control.Lens (Prism', (#))
import Control.Lens.TH (makePrisms)
import Control.Monad.Error.Class (MonadError(throwError))
import Crypto.JOSE.Error as Jose
import Data.Aeson.Types
import Data.Typeable
import Data.Validity
import Servant
import Servant.Job.Core (HasServerError(..), serverError)
import Servant.Job.Async (HasJobEnv)
import Gargantext.Prelude
import Gargantext.API.Settings
import Gargantext.API.Orchestrator.Types
import Gargantext.API.Ngrams
import Gargantext.Core.Types
import Gargantext.Database.Types.Errors (NodeError(..), HasNodeError(..))
import Gargantext.Database.Tree
import Gargantext.Database.Utils
import Gargantext.Database.Schema.Node

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
    , HasJobEnv    env ScraperStatus ScraperStatus
    )

type GargServerT env err m api = GargServerC env err m => ServerT api m

type GargServer api =
  forall env err m. GargServerT env err m api

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
