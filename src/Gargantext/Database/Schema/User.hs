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
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE Arrows                      #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RankNTypes                  #-}

module Gargantext.Database.Schema.User where

import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Show(Show(..))
import Gargantext.Prelude
import Opaleye

------------------------------------------------------------------------
------------------------------------------------------------------------

data UserLight = UserLight { userLight_id   :: Int
                           , userLight_username :: Text
                           , userLight_email    :: Text
                           } deriving (Show)

toUserLight :: UserDB -> UserLight
toUserLight (UserDB id _ _ _ u _ _ e _ _ _ ) = UserLight id u e

data UserPoly id pass llogin suser
              uname fname lname
              mail staff active djoined =
    UserDB { user_id          :: id
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

type UserReadNull = UserPoly     (Column (Nullable PGInt4))         (Column (Nullable PGText))
                                 (Column (Nullable PGTimestamptz))  (Column (Nullable PGBool))
                                 (Column (Nullable PGText))         (Column (Nullable PGText))
                                 (Column (Nullable PGText))         (Column (Nullable PGText))
                                 (Column (Nullable PGBool))         (Column (Nullable PGBool))
                                 (Column (Nullable PGTimestamptz))




type UserDB = UserPoly Int Text (Maybe UTCTime) Bool Text Text Text Text Bool Bool UTCTime

$(makeAdaptorAndInstance "pUserDB"   ''UserPoly)
$(makeLensesWith abbreviatedFields   ''UserPoly)


userTable :: Table UserWrite UserRead
userTable = Table "auth_user" (pUserDB UserDB { user_id      = optional "id"
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

