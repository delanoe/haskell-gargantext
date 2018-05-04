{-|
Module      : Gargantext.Database.user
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}


{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE Arrows                      #-}
{-# LANGUAGE NoImplicitPrelude           #-}

module Gargantext.Database.User where

import GHC.Show(Show(..))
import Data.Eq(Eq(..))
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye

-- Functions only
import Data.List (find)

import Gargantext.Prelude


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
                                 (Column PGTimestamptz)

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
                                          , user_dateJoined  = required "date_joined"
                                          }
                              )


queryUserTable :: Query UserRead
queryUserTable = queryTable userTable


selectUsersLight :: Query UserRead
selectUsersLight = proc () -> do
      row@(User i _p _ll _is _un _fn _ln _m _iff _ive _dj) <- queryUserTable -< ()
      restrict -< i .== 1
      --returnA -< User i p ll is un fn ln m iff ive dj
      returnA -< row


userWith :: (Eq a1, Foldable t) => (a -> a1) -> a1 -> t a -> Maybe a
userWith f t xs = find (\x -> f x == t) xs

userWithUsername :: Text -> [User] -> Maybe User
userWithUsername t xs = userWith user_username t xs

userWithId :: Int -> [User] -> Maybe User
userWithId t xs = userWith user_id t xs

instance QueryRunnerColumnDefault PGTimestamptz (Maybe UTCTime) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn


users :: PGS.Connection -> IO [User]
users conn = runQuery conn queryUserTable

usersLight :: PGS.Connection -> IO [UserLight]
usersLight conn = map toUserLight <$> runQuery conn queryUserTable
