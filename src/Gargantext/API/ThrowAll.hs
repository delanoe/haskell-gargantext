{-|
Module      : Gargantext.API.ThrowAll
Description : ThrowAll class and instance
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Gargantext.API.ThrowAll where

import Control.Monad.Except (MonadError(..))
import Control.Lens ((#))
import Servant
import Servant.Auth.Server (AuthResult(..))

import Gargantext.Prelude
import Gargantext.API.Prelude (GargServerM, _ServerError)
import Gargantext.API.Routes (GargPrivateAPI, serverPrivateGargAPI')

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

serverPrivateGargAPI :: MimeRender JSON err => GargServerM env err GargPrivateAPI
serverPrivateGargAPI (Authenticated auser) = serverPrivateGargAPI' auser
serverPrivateGargAPI _                     = throwAll' (_ServerError # err401)
-- Here throwAll' requires a concrete type for the monad.
