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
        Nothing        -> panic "[G.D.A.U.N.newUserQuick]: Email invalid"
  pure (NewUser u n (GargPassword pass))

------------------------------------------------------------------------
isEmail :: Text -> Bool
isEmail = ((==) 2) . List.length . (splitOn "@")

guessUserName :: Text -> Maybe (Text,Text)
guessUserName n = case splitOn "@" n of
    [u',m'] -> if m' /= "" then Just (u',m')
                           else Nothing
    _       -> Nothing
------------------------------------------------------------------------
newUser' :: HasNodeError err
        => ServerAdress -> NewUser GargPassword -> Cmd err Int64
newUser' address u = newUsers' address [u]

newUsers' :: HasNodeError err
         => ServerAdress -> [NewUser GargPassword] -> Cmd err Int64
newUsers' address us = do
  us' <- liftBase         $ mapM toUserHash us
  r   <- insertUsers      $ map toUserWrite us'
  _   <- mapM getOrMkRoot $ map (\u -> UserName (_nu_username u)) us
  _   <- liftBase         $ mapM (mail address Invitation) us
  pure r
------------------------------------------------------------------------

data SendEmail = SendEmail Bool

updateUser :: HasNodeError err
           => SendEmail -> Text -> NewUser GargPassword -> Cmd err Int64
updateUser (SendEmail send) server u = do
  u' <- liftBase     $ toUserHash   u
  n  <- updateUserDB $ toUserWrite  u'
  _  <- case send of
     True  -> liftBase     $ mail server Update u
     False -> pure ()
  pure n

------------------------------------------------------------------------
type ServerAdress = Text
data MailModel = Invitation
               | Update


-- TODO gargantext.ini config
mail :: ServerAdress -> MailModel -> NewUser GargPassword -> IO ()
mail server model user@(NewUser u m _) = gargMail (GargMail m (Just u) subject body)
  where
    subject = "[Your Garg Account]"
    body    = emailWith server model user


emailWith :: ServerAdress -> MailModel -> NewUser GargPassword -> Text
emailWith server model (NewUser u _ (GargPassword p)) = unlines $
          [ "Hello" ]
          <> bodyWith model <>
          [ ""
          , "You can log in to: " <> server
          , "Your username is: "  <> u
          , "Your password is: "  <> p
          , ""
          ]
          <> email_disclaimer
          <> email_signature

bodyWith :: MailModel -> [Text]
bodyWith Invitation = [ "Congratulation, you have been granted a beta user account to test the"
                      , "new GarganText platform!"
                      ]
bodyWith Update     = [ "Your account password have been updated on the GarganText platform!"
                      ]

email_disclaimer :: [Text]
email_disclaimer =
            [ "If you log in you agree with the following terms of use:"
            , "          https://gitlab.iscpif.fr/humanities/tofu/tree/master"
            , ""
            , ""
            , "/!\\ Please note that this account is opened for beta tester only. Hence"
            , "we cannot guarantee neither the perenniality nor the stability of the"
            , "service at this stage. It is therefore advisable to back up important"
            , "data regularly."
            , ""
            , "/!\\ Gargantext is an academic service supported by ISC-PIF partners."
            , "In case of congestion on this service, access to members of the ISC-PIF"
            , "partners will be privileged."
            , ""
            , "Your feedback will be valuable for further development of the platform,"
            , "do not hesitate to contact us and to contribute on our forum:"
            , ""
            , "     https://discourse.iscpif.fr/c/gargantext"
            , ""
            ]

email_signature :: [Text]
email_signature =
          [ "With our best regards,"
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
