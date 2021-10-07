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


{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE Arrows                      #-}
{-# LANGUAGE TemplateHaskell             #-}

module Gargantext.Database.Schema.User where

import Data.Morpheus.Types (GQLType)
import Data.Text (Text)
import Data.Time (UTCTime)
import Gargantext.Prelude
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Data.Aeson.TH (deriveJSON)
import Gargantext.Database.Prelude (fromField')
import Gargantext.Core.Utils.Prefix (unPrefix)

-- FIXME PLZ : the import below leads to an error, why ?
-- import Gargantext.Database.Schema.Prelude hiding (makeLensesWith, abbreviatedFields, makeAdaptorAndInstance)

-- When FIXED : Imports to remove:
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye hiding (FromField)

------------------------------------------------------------------------
data UserLight = UserLight { userLight_id       :: !Int
                           , userLight_username :: !Text
                           , userLight_email    :: !Text
                           , userLight_password :: !Text
                           } deriving (Show, Generic, GQLType)

toUserLight :: UserDB -> UserLight
toUserLight (UserDB id p _ _ u _ _ e _ _ _ ) = UserLight id u e p


data UserPoly id pass llogin suser
              uname fname lname
              mail staff active djoined =
    UserDB { user_id          :: !id
           , user_password    :: !pass
           , user_lastLogin   :: !llogin
           , user_isSuperUser :: !suser

           , user_username    :: !uname
           , user_firstName   :: !fname
           , user_lastName    :: !lname
           , user_email       :: !mail

           , user_isStaff     :: !staff
           , user_isActive    :: !active
           , user_dateJoined  :: !djoined
           } deriving (Show, Generic)


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
userTable = Table "auth_user"
  (pUserDB UserDB { user_id      = optionalTableField "id"
                  , user_password    = requiredTableField "password"
                  , user_lastLogin   = optionalTableField "last_login"
                  , user_isSuperUser = requiredTableField "is_superuser"
                  , user_username    = requiredTableField "username"
                  , user_firstName   = requiredTableField "first_name"
                  , user_lastName    = requiredTableField "last_name"
                  , user_email       = requiredTableField "email"
                  , user_isStaff     = requiredTableField "is_staff"
                  , user_isActive    = requiredTableField "is_active"
                  , user_dateJoined  = optionalTableField "date_joined"
                  }
      )

instance FromField UserLight where
  fromField = fromField'

instance FromField UserDB where
  fromField = fromField'

$(deriveJSON (unPrefix "userLight_") ''UserLight)
$(deriveJSON (unPrefix "user_") ''UserPoly)
