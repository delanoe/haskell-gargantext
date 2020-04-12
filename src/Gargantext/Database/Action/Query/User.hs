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

module Gargantext.Database.Action.Query.User
  where

import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Eq(Eq(..))
import Data.List (find)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Show(Show(..))
import Gargantext.Core.Types.Individu (Username, arbitraryUsername, User(..), UserId)
import Gargantext.Database.Admin.Types.Errors
import Gargantext.Database.Schema.User
import Gargantext.Database.Admin.Utils
import Gargantext.Prelude
import Opaleye

------------------------------------------------------------------------
-- TODO: on conflict, nice message
insertUsers :: [UserWrite] -> Cmd err Int64
insertUsers us = mkCmd $ \c -> runInsert_ c insert
  where
    insert = Insert userTable us rCount Nothing


gargantextUser :: Username -> UserWrite
gargantextUser u = UserDB (Nothing) (pgStrictText "password")
                         (Nothing) (pgBool True) (pgStrictText u)
                         (pgStrictText "first_name")
                         (pgStrictText "last_name")
                         (pgStrictText "e@mail")
                         (pgBool True) (pgBool True) (Nothing)

insertUsersDemo :: Cmd err Int64
insertUsersDemo = insertUsers $ map (\u -> gargantextUser u) arbitraryUsername

------------------------------------------------------------------
queryUserTable :: Query UserRead
queryUserTable = queryTable userTable

selectUsersLight :: Query UserRead
selectUsersLight = proc () -> do
      row@(UserDB i _p _ll _is _un _fn _ln _m _iff _ive _dj) <- queryUserTable -< ()
      restrict -< i .== 1
      --returnA -< User i p ll is un fn ln m iff ive dj
      returnA -< row
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

