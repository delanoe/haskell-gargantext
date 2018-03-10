{-|
Module      : Gargantext.API.Foundation
Description : Handler of the API (Server and Client)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Inspired by : http://blog.wuzzeb.org/full-stack-web-haskell/server.html

-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE AllowAmbiguousTypes         #-}

------------------------------------------------------------------------
module Gargantext.API.Foundation
    where
------------------------------------------------------------------------
import System.Log.FastLogger

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
-- import Control.Natural ((:~>))

import Data.Maybe

-- import Database.PostgreSQL.Simple
import Servant

-- import Gargantext.API
import Gargantext.API.Auth
import Gargantext.API.Settings
import Gargantext.Prelude
------------------------------------------------------------------------
------------------------------------------------------------------------

newtype MyServer a = MyServer { myServerM :: 
                                    ReaderT   (Env, Maybe Token     )
                                  ( ResourceT (ExceptT ServantErr IO) )
                                    a
                              }
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadError ServantErr MyServer

instance MonadBase IO MyServer where liftBase = liftIO

instance MonadReader Env MyServer where
  ask = MyServer (fst <$> ask)
  local f (MyServer r) = MyServer (local (\(e,t) -> (f e, t)) r)

instance MonadLogger MyServer where
    monadLoggerLog loc source ll msg = do
        limit <- view (settings.logLevelLimit)
        out   <- view logger
        when (ll >= limit) $
            liftIO $ pushLogStr out $ defaultLogStr loc source ll $ toLogStr msg

------------------------------------------------------------------------
getToken :: MyServer (Maybe Token)
getToken  = MyServer (snd <$> ask)

userRequired :: MyServer UserCredentials
userRequired  = do
    mt <- getToken
    case mt of
        Nothing -> throwError $ err401 { errBody = "No Authorization header in request" }
        Just t  -> return $ t^.tokenCreds

runDB :: (a -> b) -> MyServer b
runDB _ = undefined {- access pool from env, run action -}
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--
-- | very basic Example for testing purpose

-- type MyAPI         = TeamAPI -- :<|> UserAPI :<|> ...
type TeamAPI       = GetUserRoute -- :<|>
type GetUserRoute  = "team"  :> Capture "teamkey" Int :> Get '[JSON] Int
--type GetTeamRoute     = "team"  :> Capture "teamkey" TeamKey :> Get '[JSON] Team

myServerAPI :: Proxy TeamAPI
myServerAPI = Proxy

--gargServer' :: ServerT MyAPI MyServer
--gargServer'  = teamServer

teamServer :: ServerT TeamAPI MyServer
teamServer  = getTeamR -- :<|> createTeamR :<|> updateTeamR :<|> getAllTeamsR

getTeamR  :: Int -> MyServer Int
getTeamR _ = do
    pure 1

---- Note that @type MyAPIWithAuth = JwtAuthHeader :> MyAPI@ so that
---- @Server MyAPIWithAuth@ expands to @Maybe UnverifiedJwtToken -> Server MyAPI@.
--myServerWithAuth :: Env -> Server MyAPIWithAuth
----myServerWithAuth env unverifiedJwt = enter (myServerNat env unverifiedJwt) myServer
--myServerWithAuth :: forall a. Env -> Maybe UnverifiedJwtToken -> ServerT (MyServer a) Handler

myServerWithAuth :: Env -> Maybe UnverifiedJwtToken -> Int -> Handler Int
myServerWithAuth env unverifiedJwt = hoistServer myServerAPI (nt env unverifiedJwt) teamServer

-- nt :: Applicative f => Env -> p -> MyServer a -> f (ExceptT ServantErr IO a)
nt :: Env -> Maybe UnverifiedJwtToken -> MyServer a -> Handler (ExceptT ServantErr IO a)
nt env _ s = pure $ runResourceT (runReaderT (myServerM s) (env, mtoken'))
   where
       mtoken' :: Maybe Token
       mtoken'  = undefined

