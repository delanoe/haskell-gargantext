{-|
Module      : Gargantext.Database.Action.User
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Database.Action.User
    where

import Data.Text (Text)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.User
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Schema.Node
import Gargantext.Prelude

------------------------------------------------------------------------
getUserLightWithId :: HasNodeError err => Int -> Cmd err UserLight
getUserLightWithId i = do
  candidates <- head <$> getUsersWithId i
  case candidates of
    Nothing -> nodeError NoUserFound
    Just u  -> pure u

getUserLightDB :: HasNodeError err => User -> Cmd err UserLight
getUserLightDB u = do
  userId <- getUserId u
  userLight <- getUserLightWithId userId
  pure userLight

------------------------------------------------------------------------
getUserId :: HasNodeError err
          => User
          -> Cmd err UserId
getUserId u = do
  maybeUser <- getUserId' u
  case maybeUser of
    Nothing -> nodeError NoUserFound
    Just u'  -> pure u'

getUserId' :: HasNodeError err
          => User
          -> Cmd err (Maybe UserId)
getUserId' (UserDBId uid) = pure (Just uid)
getUserId' (RootId   rid) = do
  n <- getNode rid
  pure $ Just $ _node_user_id n
getUserId' (UserName u  ) = do
  muser <- getUser u
  case muser of
    Just user -> pure $ Just $ userLight_id user
    Nothing   -> pure Nothing
getUserId' UserPublic = pure Nothing

------------------------------------------------------------------------
-- | Username = Text
-- UserName is User
-- that is confusing, we should change this
type Username = Text
getUsername :: HasNodeError err
            => User
            -> Cmd err Username
getUsername (UserName u) = pure u
getUsername (UserDBId i) = do
  users <- getUsersWithId i
  case head users of
    Just u  -> pure $ userLight_username u
    Nothing -> nodeError $ NodeError "G.D.A.U.getUserName: User not found with that id"
getUsername (RootId   rid) = do
  n <- getNode rid
  getUsername (UserDBId $ _node_user_id n)
getUsername UserPublic = pure "UserPublic"

--------------------------------------------------------------------------
-- getRootId is in Gargantext.Database.Query.Tree.Root

