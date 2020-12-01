{-|
Module      : Gargantext.Database.Action.User.New
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

module Gargantext.Database.Action.User.New
  where

import Control.Lens (view)
import Control.Monad.Random
import Data.Text (Text, unlines, splitOn)
import Gargantext.Core.Types.Individu
import Gargantext.Database.Action.Flow (getOrMkRoot)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..), nodeError, NodeError(..))
import Gargantext.Database.Query.Table.User
import Gargantext.Prelude
import Gargantext.Prelude.Config
import Gargantext.Prelude.Crypto.Pass.User (gargPass)
import Gargantext.Prelude.Mail (gargMail, GargMail(..))
import qualified Data.List as List

------------------------------------------------------------------------
type EmailAddress = Text
------------------------------------------------------------------------
newUsers :: (CmdM env err m, MonadRandom m, HasNodeError err)
         => [EmailAddress] -> m Int64
newUsers us = do
  us' <- mapM newUserQuick us
  url <- view $ config . gc_url
  newUsers' url us'
------------------------------------------------------------------------
newUserQuick :: (MonadRandom m)
             => Text -> m (NewUser GargPassword)
newUserQuick n = do
  pass <- gargPass
  let u = case guessUserName n of
        Just  (u', _m) -> u'
        Nothing        -> panic "Email invalid"
  pure (NewUser u n (GargPassword pass))

isEmail :: Text -> Bool
isEmail = ((==) 2) . List.length . (splitOn "@")

guessUserName :: Text -> Maybe (Text,Text)
guessUserName n = case splitOn "@" n of
    [u',m'] -> if m' /= "" then Just (u',m')
                           else Nothing
    _       -> Nothing
------------------------------------------------------------------------
newUser' :: HasNodeError err
        => Text -> NewUser GargPassword -> Cmd err Int64
newUser' address u = newUsers' address [u]

newUsers' :: HasNodeError err
         => Text -> [NewUser GargPassword] -> Cmd err Int64
newUsers' address us = do
  us' <- liftBase         $ mapM toUserHash us
  r   <- insertUsers      $ map toUserWrite us'
  _   <- mapM getOrMkRoot $ map (\u -> UserName (_nu_username u)) us
  _   <- liftBase         $ mapM (mail Invitation address) us
  pure r
------------------------------------------------------------------------
updateUser :: HasNodeError err
           => Text -> NewUser GargPassword -> Cmd err Int64
updateUser address u = do
  u' <- liftBase     $ toUserHash   u
  n  <- updateUserDB $ toUserWrite  u'
  _  <- liftBase     $ mail Update address u
  pure n

------------------------------------------------------------------------
data Mail = Invitation
          | Update


-- TODO gargantext.ini config
mail :: Mail -> Text -> NewUser GargPassword -> IO ()
mail mtype address nu@(NewUser u m _) = gargMail (GargMail m (Just u) subject body)
  where
    subject = "[Your Garg Account]"
    body    = bodyWith mtype address nu

bodyWith :: Mail -> Text -> NewUser GargPassword -> Text
bodyWith Invitation add nu = logInstructions    add nu
bodyWith Update     add nu = updateInstructions add nu


-- TODO put this in a configurable file (path in gargantext.ini)
logInstructions :: Text -> NewUser GargPassword -> Text
logInstructions address (NewUser u _ (GargPassword p)) =
  unlines [ "Hello"
          , "You have been invited to test the new GarganText platform!"
          , ""
          , "You can log in to: " <> address
          , "Your username is: "  <> u
          , "Your password is: "  <> p
          , ""
          , "Please read the full terms of use on:"
          , "https://gitlab.iscpif.fr/humanities/tofu/tree/master"
          , ""
          , "Your feedback will be valuable for further development"
          , "of the platform, do not hesitate to contact us and"
          , "to contribute on our forum:"
          , "     https://discourse.iscpif.fr/c/gargantext"
          , ""
          , "With our best regards,"
          , "-- "
          , "The Gargantext Team (CNRS)"
          ]

updateInstructions :: Text -> NewUser GargPassword -> Text
updateInstructions address (NewUser u _ (GargPassword p)) =
  unlines [ "Hello"
          , "Your account have been updated on the GarganText platform!"
          , ""
          , "You can log in to: " <> address
          , "Your username is: "  <> u
          , "Your password is: "  <> p
          , ""
          , "As reminder, please read the full terms of use on:"
          , "https://gitlab.iscpif.fr/humanities/tofu/tree/master"
          , ""
          , "Your feedback is always valuable for further development"
          , "of the platform, do not hesitate to contact us and"
          , "to contribute on our forum:"
          , "     https://discourse.iscpif.fr/c/gargantext"
          , ""
          , "With our best regards,"
          , "-- "
          , "The Gargantext Team (CNRS)"
          ]


------------------------------------------------------------------------
rmUser :: HasNodeError err => User -> Cmd err Int64
rmUser (UserName un) = deleteUsers [un]
rmUser _ = nodeError NotImplYet

-- TODO
rmUsers :: HasNodeError err => [User] -> Cmd err Int64
rmUsers [] = pure 0
rmUsers _  = undefined
