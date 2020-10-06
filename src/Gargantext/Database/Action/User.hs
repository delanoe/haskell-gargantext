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
getUserId :: HasNodeError err
          => User
          -> Cmd err UserId
getUserId (UserDBId uid) = pure uid
getUserId (RootId   rid) = do
  n <- getNode rid
  pure $ _node_userId n
getUserId (UserName u  ) = do
  muser <- getUser u
  case muser of
    Just user -> pure $ userLight_id user
    Nothing   -> nodeError NoUserFound
getUserId UserPublic = nodeError NoUserFound

------------------------------------------------------------------------
-- | Username = Text
-- UserName is User
-- that is confusing, we should change this
getUsername :: HasNodeError err
            => User
            -> Cmd err Text
getUsername (UserName u) = pure u
getUsername (UserDBId i) = do
  users <- getUsersWithId i
  case head users of
    Just u  -> pure $ userLight_username u
    Nothing -> nodeError $ NodeError "G.D.A.U.getUserName: User not found with that id"
getUsername (RootId   rid) = do
  n <- getNode rid
  getUsername (UserDBId $ _node_userId n)
getUsername UserPublic = pure "UserPublic"

--------------------------------------------------------------------------
-- getRootId is in Gargantext.Database.Query.Tree.Root

