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


{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE Arrows                      #-}

module Gargantext.Database.Query.Table.User
  ( insertUsers
  , toUserWrite
  , deleteUsers
  , updateUserDB
  , queryUserTable
  , getUserHyperdata
  , getUsersWithHyperdata
  , getUsersWithNodeHyperdata
  , updateUserEmail
  , updateUserForgotPasswordUUID
  , getUser
  , insertNewUsers
  , selectUsersLightWith
  , userWithUsername
  , userWithId
  , userLightWithId
  , getUsersWith
  , getUsersWithEmail
  , getUsersWithForgotPasswordUUID
  , getUsersWithId
  , module Gargantext.Database.Schema.User
  )
  where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.List (find)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import Gargantext.Core.Types.Individu
import qualified Gargantext.Prelude.Crypto.Auth as Auth
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataUser)
import Gargantext.Database.Admin.Types.Node (NodeType(NodeUser), Node)
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Node (NodeRead, node_hyperdata, queryNodeTable, node_user_id, node_typename)
import Gargantext.Database.Schema.User
import Gargantext.Prelude
import Opaleye

------------------------------------------------------------------------
-- TODO: on conflict, nice message
insertUsers :: [UserWrite] -> Cmd err Int64
insertUsers us = mkCmd $ \c -> runInsert_ c insert
  where
    insert = Insert userTable us rCount Nothing

deleteUsers :: [Username] -> Cmd err Int64
deleteUsers us = mkCmd $ \c -> runDelete_ c
                       $ Delete userTable
                                (\user -> in_ (map sqlStrictText us) (user_username user))
                                rCount

-- Updates email or password only (for now)
updateUserDB :: UserWrite -> Cmd err Int64
updateUserDB us = mkCmd $ \c -> runUpdate_ c (updateUserQuery us)
  where
    updateUserQuery :: UserWrite -> Update Int64
    updateUserQuery us' = Update
      { uTable      = userTable
      , uUpdateWith = updateEasy (\ (UserDB { .. })
                                  -> UserDB { user_password = p'
                                            , user_email = em'
                                            , .. }
                                 )
      , uWhere      = (\row -> user_username row .== un')
      , uReturning  = rCount
      }
        where
          UserDB { user_password = p'
                 , user_username = un'
                 , user_email = em' } = us'

-----------------------------------------------------------------------
toUserWrite :: NewUser HashPassword -> UserWrite
toUserWrite (NewUser u m (Auth.PasswordHash p)) = 
  UserDB { user_id = Nothing
         , user_password = sqlStrictText p
         , user_lastLogin = Nothing
         , user_isSuperUser = sqlBool True
         , user_username = sqlStrictText u
         , user_firstName = sqlStrictText "first_name"
         , user_lastName = sqlStrictText "last_name"
         , user_email = sqlStrictText m
         , user_isStaff = sqlBool True
         , user_isActive = sqlBool True
         , user_dateJoined = Nothing
         , user_forgot_password_uuid = Nothing }

------------------------------------------------------------------
getUsersWith :: Username -> Cmd err [UserLight]
getUsersWith u = map toUserLight <$> runOpaQuery (selectUsersLightWith u)

selectUsersLightWith :: Username -> Select UserRead
selectUsersLightWith u = proc () -> do
      row      <- queryUserTable -< ()
      restrict -< user_username row .== sqlStrictText u
      returnA  -< row

getUsersWithEmail :: Text -> Cmd err [UserLight]
getUsersWithEmail e = map toUserLight <$> runOpaQuery (selectUsersLightWithEmail e)

selectUsersLightWithEmail :: Text -> Select UserRead
selectUsersLightWithEmail e = proc () -> do
      row      <- queryUserTable -< ()
      restrict -< user_email row .== sqlStrictText e
      returnA  -< row

getUsersWithForgotPasswordUUID :: UUID.UUID -> Cmd err [UserLight]
getUsersWithForgotPasswordUUID uuid = map toUserLight <$> runOpaQuery (selectUsersLightWithForgotPasswordUUID uuid)

selectUsersLightWithForgotPasswordUUID :: UUID.UUID -> Select UserRead
selectUsersLightWithForgotPasswordUUID uuid = proc () -> do
      row      <- queryUserTable -< ()
      restrict -< user_forgot_password_uuid row .== sqlStrictText (UUID.toText uuid)
      returnA  -< row

----------------------------------------------------------
getUsersWithId :: Int -> Cmd err [UserLight]
getUsersWithId i = map toUserLight <$> runOpaQuery (selectUsersLightWithId i)
  where
    selectUsersLightWithId :: Int -> Select UserRead
    selectUsersLightWithId i' = proc () -> do
          row      <- queryUserTable -< ()
          restrict -< user_id row .== sqlInt4 i'
          returnA  -< row


queryUserTable :: Select UserRead
queryUserTable = selectTable userTable

----------------------------------------------------------------------
getUserHyperdata :: Int -> Cmd err [HyperdataUser]
getUserHyperdata i = do
  runOpaQuery (selectUserHyperdataWithId i)
  where
    selectUserHyperdataWithId :: Int -> Select (Column SqlJsonb)
    selectUserHyperdataWithId i' = proc () -> do
      row      <- queryNodeTable -< ()
      restrict -< row^.node_user_id .== (sqlInt4 i')
      restrict -< row^.node_typename .== (sqlInt4 $ nodeTypeId NodeUser)
      returnA  -< row^.node_hyperdata

getUserNodeHyperdata :: Int -> Cmd err [Node HyperdataUser]
getUserNodeHyperdata i = do
  runOpaQuery (selectUserHyperdataWithId i)
  where
    selectUserHyperdataWithId :: Int -> Select NodeRead
    selectUserHyperdataWithId i' = proc () -> do
      row      <- queryNodeTable -< ()
      restrict -< row^.node_user_id .== (sqlInt4 i')
      restrict -< row^.node_typename .== (sqlInt4 $ nodeTypeId NodeUser)
      returnA  -< row



getUsersWithHyperdata :: Int -> Cmd err [(UserLight, HyperdataUser)]
getUsersWithHyperdata i = do
  u <- getUsersWithId i
  h <- getUserHyperdata i
  -- printDebug "[getUsersWithHyperdata]" (u,h)
  pure $ zip u h

getUsersWithNodeHyperdata :: Int -> Cmd err [(UserLight, Node HyperdataUser)]
getUsersWithNodeHyperdata i = do
  u <- getUsersWithId i
  h <- getUserNodeHyperdata i
  -- printDebug "[getUsersWithHyperdata]" (u,h)
  pure $ zip u h


updateUserEmail :: UserLight -> Cmd err Int64
updateUserEmail (UserLight { .. }) = mkCmd $ \c -> runUpdate_ c updateUserQuery
  where
    updateUserQuery :: Update Int64
    updateUserQuery = Update
      { uTable      = userTable
      , uUpdateWith = updateEasy (\ (UserDB { .. }) -> UserDB { user_email = sqlStrictText userLight_email, .. } )
      , uWhere      = (\row -> user_id row .== (sqlInt4 userLight_id))
      , uReturning  = rCount }


updateUserForgotPasswordUUID :: UserLight -> UUID.UUID -> Cmd err Int64
updateUserForgotPasswordUUID (UserLight { .. }) uuid = mkCmd $ \c -> runUpdate_ c updateUserQuery
  where
    updateUserQuery :: Update Int64
    updateUserQuery = Update
      { uTable      = userTable
      , uUpdateWith = updateEasy (\ (UserDB { .. }) -> UserDB { user_forgot_password_uuid = sqlStrictText $ UUID.toText uuid, .. })
      , uWhere      = (\row -> user_id row .== (sqlInt4 userLight_id))
      , uReturning  = rCount }
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
----------------------------------------------------------------------
users :: Cmd err [UserDB]
users = runOpaQuery queryUserTable

usersLight :: Cmd err [UserLight]
usersLight = map toUserLight <$> users

getUser :: Username -> Cmd err (Maybe UserLight)
getUser u = userLightWithUsername u <$> usersLight

----------------------------------------------------------------------
insertNewUsers :: [NewUser GargPassword] -> Cmd err Int64
insertNewUsers newUsers = do
  users' <- liftBase $ mapM toUserHash newUsers
  insertUsers $ map toUserWrite users'

----------------------------------------------------------------------
instance DefaultFromField SqlTimestamptz (Maybe UTCTime) where
  defaultFromField = fromPGSFromField
