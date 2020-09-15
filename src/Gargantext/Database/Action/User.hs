{-|
Module      : Gargantext.Database.Action.User
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

module Gargantext.Database.Action.User
  where

-- import Data.Maybe (catMaybes)
import Data.Text (Text, unlines)
import Gargantext.Database.Query.Table.User
import Gargantext.Core.Types.Individu
import Gargantext.Database.Prelude
import Gargantext.Prelude
import Gargantext.Prelude.Mail (gargMail, GargMail(..))
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..), nodeError, NodeError(..))
import Gargantext.Database.Action.Flow (getOrMkRoot)


------------------------------------------------------------------------
mkUser :: HasNodeError err => Text -> NewUser GargPassword -> Cmd err Int64
mkUser address u = mkUsers address [u]

mkUsers :: HasNodeError err => Text -> [NewUser GargPassword] -> Cmd err Int64
mkUsers address us = do
  us' <- liftBase    $ mapM toUserHash us
  r   <- insertUsers $ map toUserWrite us'
  _   <- mapM getOrMkRoot $ map (\u -> UserName (_nu_username u)) us
  _   <- liftBase    $ mapM (mail address) us
  pure r

------------------------------------------------------------------------
-- TODO gargantext.ini config
mail :: Text -> NewUser GargPassword -> IO ()
mail address nu@(NewUser u m _) = gargMail (GargMail m (Just u) subject body)
  where
    subject = "[Your Garg Account]"
    body    = logInstructions address nu

-- TODO put this in a configurable file (path in gargantext.ini)
logInstructions :: Text -> NewUser GargPassword -> Text
logInstructions address (NewUser u _ (GargPassword p)) =
  unlines [ "Hello"
          , "You have been invited to test the new GarganText platform!"
          , ""
          , "You can log on to: "  <> address
          , "Your login is: "      <> u
          , "Your password is: "   <> p
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

------------------------------------------------------------------------


------------------------------------------------------------------------
rmUser :: HasNodeError err => User -> Cmd err Int64
rmUser (UserName un) = deleteUsers [un]
rmUser _ = nodeError NotImplYet

-- TODO
rmUsers :: HasNodeError err => [User] -> Cmd err Int64
rmUsers [] = pure 0
rmUsers _  = undefined
