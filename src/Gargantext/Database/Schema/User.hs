{-|
Module      : Gargantext.Database.user
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
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE Arrows                      #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RankNTypes                  #-}

module Gargantext.Database.Schema.User where

import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Eq(Eq(..))
import Data.List (find)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Show(Show(..))
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Database.Utils
import Gargantext.Prelude
import Opaleye

------------------------------------------------------------------------
type UserId = Int


data UserLight = UserLight { userLight_id   :: Int
                           , userLight_username :: Text
                           , userLight_email    :: Text
                           } deriving (Show)

toUserLight :: User -> UserLight
toUserLight (User id _ _ _ u _ _ e _ _ _ ) = UserLight id u e

data UserPoly id pass llogin suser
              uname fname lname
              mail staff active djoined = User { user_id          :: id
                                               , user_password    :: pass
                                               , user_lastLogin   :: llogin
                                               , user_isSuperUser :: suser

                                               , user_username    :: uname
                                               , user_firstName   :: fname
                                               , user_lastName    :: lname
                                               , user_email       :: mail

                                               , user_isStaff     :: staff
                                               , user_isActive    :: active
                                               , user_dateJoined  :: djoined
                                               } deriving (Show)

type UserWrite = UserPoly (Maybe (Column PGInt4))        (Column PGText)
                          (Maybe (Column PGTimestamptz)) (Column PGBool)
                                 (Column PGText)         (Column PGText)
                                 (Column PGText)         (Column PGText)
                                 (Column PGBool)         (Column PGBool)
                                 (Maybe (Column PGTimestamptz))

type UserRead  = UserPoly        (Column PGInt4)         (Column PGText)
                                 (Column PGTimestamptz)  (Column PGBool)
                                 (Column PGText)         (Column PGText)
                                 (Column PGText)         (Column PGText)
                                 (Column PGBool)         (Column PGBool)
                                 (Column PGTimestamptz)

type User = UserPoly Int Text (Maybe UTCTime) Bool Text Text Text Text Bool Bool UTCTime

$(makeAdaptorAndInstance "pUser"     ''UserPoly)
$(makeLensesWith abbreviatedFields   ''UserPoly)


userTable :: Table UserWrite UserRead
userTable = Table "auth_user" (pUser User { user_id      = optional "id"
                                          , user_password    = required "password"
                                          , user_lastLogin   = optional "last_login"
                                          , user_isSuperUser = required "is_superuser"
                                          , user_username    = required "username"
                                          , user_firstName   = required "first_name"
                                          , user_lastName    = required "last_name"
                                          , user_email       = required "email"
                                          , user_isStaff     = required "is_staff"
                                          , user_isActive    = required "is_active"
                                          , user_dateJoined  = optional "date_joined"
                                          }
                              )

-- TODO: on conflict, nice message
insertUsers :: [UserWrite] -> Cmd err Int64
insertUsers us = mkCmd $ \c -> runInsertMany c userTable us

gargantuaUser :: UserWrite
gargantuaUser = User (Nothing) (pgStrictText "password")
                         (Nothing) (pgBool True) (pgStrictText "gargantua")
                         (pgStrictText "first_name")
                         (pgStrictText "last_name")
                         (pgStrictText "e@mail")
                         (pgBool True) (pgBool True) (Nothing)

simpleUser :: UserWrite
simpleUser = User (Nothing) (pgStrictText "password")
                         (Nothing) (pgBool False) (pgStrictText "user1")
                         (pgStrictText "first_name")
                         (pgStrictText "last_name")
                         (pgStrictText "e@mail")
                         (pgBool False) (pgBool True) (Nothing)


------------------------------------------------------------------
queryUserTable :: Query UserRead
queryUserTable = queryTable userTable

selectUsersLight :: Query UserRead
selectUsersLight = proc () -> do
      row@(User i _p _ll _is _un _fn _ln _m _iff _ive _dj) <- queryUserTable -< ()
      restrict -< i .== 1
      --returnA -< User i p ll is un fn ln m iff ive dj
      returnA -< row
------------------------------------------------------------------
-- | Select User with some parameters
-- Not optimized version
userWith :: (Eq a1, Foldable t) => (a -> a1) -> a1 -> t a -> Maybe a
userWith f t xs = find (\x -> f x == t) xs

-- | Select User with Username
userWithUsername :: Text -> [User] -> Maybe User
userWithUsername t xs = userWith user_username t xs

userWithId :: Int -> [User] -> Maybe User
userWithId t xs = userWith user_id t xs

userLightWithUsername :: Text -> [UserLight] -> Maybe UserLight
userLightWithUsername t xs = userWith userLight_username t xs

userLightWithId :: Int -> [UserLight] -> Maybe UserLight
userLightWithId t xs = userWith userLight_id t xs


instance QueryRunnerColumnDefault PGTimestamptz (Maybe UTCTime) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn


users :: Cmd err [User]
users = runOpaQuery queryUserTable

usersLight :: Cmd err [UserLight]
usersLight = map toUserLight <$> users

getUser :: Username -> Cmd err (Maybe UserLight)
getUser u = userLightWithUsername u <$> usersLight


