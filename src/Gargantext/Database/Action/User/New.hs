{-|
Module      : Gargantext.Database.Action.User.New
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}

module Gargantext.Database.Action.User.New
  where

import Control.Lens (view)
import Control.Monad.Random
import Data.Text (Text, splitOn)
import Gargantext.Core.Mail
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.Flow (getOrMkRoot)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..), nodeError, NodeError(..))
import Gargantext.Database.Query.Table.User
import Gargantext.Prelude
import Gargantext.Prelude.Config
import Gargantext.Prelude.Crypto.Pass.User (gargPass)
import qualified Data.Text as Text
------------------------------------------------------------------------
------------------------------------------------------------------------
newUsers :: (CmdM env err m, MonadRandom m, HasNodeError err)
         => [EmailAddress] -> m Int64
newUsers us = do
  us' <- mapM newUserQuick us
  url <- view $ hasConfig . gc_url
  newUsers' url us'
------------------------------------------------------------------------
newUserQuick :: (MonadRandom m)
             => Text -> m (NewUser GargPassword)
newUserQuick n = do
  pass <- gargPass
  let u = case guessUserName n of
        Just  (u', _m) -> u'
        Nothing        -> panic "[G.D.A.U.N.newUserQuick]: Email invalid"
  pure (NewUser u n (GargPassword pass))

------------------------------------------------------------------------
-- | guessUserName
-- guess username and normalize it (Text.toLower)
guessUserName :: Text -> Maybe (Text,Text)
guessUserName n = case splitOn "@" n of
    [u',m'] -> if m' /= "" then Just (Text.toLower u',m')
                           else Nothing
    _       -> Nothing
------------------------------------------------------------------------
newUser' :: HasNodeError err
        => ServerAddress -> NewUser GargPassword -> Cmd err Int64
newUser' address u = newUsers' address [u]

newUsers' :: HasNodeError err
         => ServerAddress -> [NewUser GargPassword] -> Cmd err Int64
newUsers' address us = do
  us' <- liftBase         $ mapM toUserHash  us
  r   <- insertUsers      $ map  toUserWrite us'
  _   <- mapM getOrMkRoot $ map  (\u -> UserName   (_nu_username u)) us
  _   <- liftBase         $ mapM (\u -> mail address (Invitation u)) us
  printDebug "newUsers'" us
  pure r
------------------------------------------------------------------------

updateUser :: HasNodeError err
           => SendEmail -> Text -> NewUser GargPassword -> Cmd err Int64
updateUser (SendEmail send) server u = do
  u' <- liftBase     $ toUserHash   u
  n  <- updateUserDB $ toUserWrite  u'
  _  <- case send of
     True  -> liftBase     $ mail server (PassUpdate u)
     False -> pure ()
  pure n

------------------------------------------------------------------------
rmUser :: HasNodeError err => User -> Cmd err Int64
rmUser (UserName un) = deleteUsers [un]
rmUser _ = nodeError NotImplYet

-- TODO
rmUsers :: HasNodeError err => [User] -> Cmd err Int64
rmUsers [] = pure 0
rmUsers _  = undefined
