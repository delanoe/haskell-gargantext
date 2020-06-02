{-|
Module      : Gargantext.Database.Query.Table.User
Description : User Database management tools
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Functions to deal with users, database side.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE Arrows                      #-}

module Gargantext.Database.Query.Table.User
  ( insertUsers
  , queryUserTable
  , getUser
  , gargUserWith
  , insertUsersDemo
  , selectUsersLightWith
  , userWithUsername
  , userWithId
  , userLightWithId
  , getUsersWith
  , module Gargantext.Database.Schema.User
  )
  where

import Control.Arrow (returnA)
import Data.Eq(Eq(..))
import Data.List (find)
import Data.Maybe (Maybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Gargantext.Core.Types.Individu
import qualified Gargantext.Core.Auth as Auth
import Gargantext.Database.Schema.User
import Gargantext.Database.Prelude
import Gargantext.Prelude
import Opaleye

------------------------------------------------------------------------


-- TODO: on conflict, nice message
insertUsers :: [UserWrite] -> Cmd err Int64
insertUsers us = mkCmd $ \c -> runInsert_ c insert
  where
    insert = Insert userTable us rCount Nothing

insertUsersDemo :: Cmd err Int64
insertUsersDemo = do
  users <- liftBase arbitraryUsersHash
  insertUsers $ map (\(u,m,h) -> gargUserWith u m h) users

-----------------------------------------------------------------------

gargUserWith :: Username -> Email -> Auth.PasswordHash Auth.Argon2 -> UserWrite
gargUserWith u m (Auth.PasswordHash p) = UserDB (Nothing) (pgStrictText p)
                         (Nothing) (pgBool True) (pgStrictText u)
                         (pgStrictText "first_name")
                         (pgStrictText "last_name")
                         (pgStrictText m)
                         (pgBool True) 
                         (pgBool True) Nothing

------------------------------------------------------------------
getUsersWith :: Username -> Cmd err [UserLight]
getUsersWith u = map toUserLight <$> runOpaQuery (selectUsersLightWith u)

selectUsersLightWith :: Username -> Query UserRead
selectUsersLightWith u = proc () -> do
      row      <- queryUserTable -< ()
      restrict -< user_username row .== pgStrictText u
      returnA  -< row

queryUserTable :: Query UserRead
queryUserTable = queryTable userTable

------------------------------------------------------------------
-- | Select User with some parameters
-- Not optimized version
userWith :: (Eq a1, Foldable t) => (a -> a1) -> a1 -> t a -> Maybe a
userWith f t xs = find (\x -> f x == t) xs

-- | Select User with Username
userWithUsername :: Text -> [UserDB] -> Maybe UserDB
userWithUsername t xs = userWith user_username t xs

userWithId :: Int -> [UserDB] -> Maybe UserDB
userWithId t xs = userWith user_id t xs

userLightWithUsername :: Text -> [UserLight] -> Maybe UserLight
userLightWithUsername t xs = userWith userLight_username t xs

userLightWithId :: Int -> [UserLight] -> Maybe UserLight
userLightWithId t xs = userWith userLight_id t xs


instance QueryRunnerColumnDefault PGTimestamptz (Maybe UTCTime) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn


users :: Cmd err [UserDB]
users = runOpaQuery queryUserTable

usersLight :: Cmd err [UserLight]
usersLight = map toUserLight <$> users

getUser :: Username -> Cmd err (Maybe UserLight)
getUser u = userLightWithUsername u <$> usersLight

