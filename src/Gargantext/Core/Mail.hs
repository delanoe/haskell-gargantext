{-|
Module      : Gargantext.Core.Mail
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Mail where

import Control.Lens (view)
import Network.URI.Encode (encodeText)
import Data.Text (Text, unlines, splitOn)
import Gargantext.Core.Types.Individu
import Gargantext.Database.Schema.User (UserLight(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config (gc_url, gc_backend_name)
import Gargantext.Database.Prelude
-- import Gargantext.Prelude.Config (gc_url)
import Gargantext.Prelude.Mail (gargMail, GargMail(..))
import Gargantext.Prelude.Mail.Types (MailConfig)
import qualified Data.List as List


-- | Tool to put elsewhere
isEmail :: Text -> Bool
isEmail = ((==) 2) . List.length . (splitOn "@")

------------------------------------------------------------------------
data SendEmail    = SendEmail Bool

type EmailAddress  = Text
type Name          = Text
data ServerAddress = ServerAddress { sa_name :: Text
                                   , sa_url  :: Text
                                   }

data MailModel = Invitation { invitation_user :: NewUser GargPassword }
               | PassUpdate { passUpdate_user :: NewUser GargPassword }
               | MailInfo   { mailInfo_username :: Name
                            , mailInfo_address  :: EmailAddress
                            }
               | ForgotPassword { user :: UserLight }
------------------------------------------------------------------------
------------------------------------------------------------------------
mail :: (CmdM env err m) => MailConfig -> MailModel -> m ()
mail mailCfg model = do
  cfg <- view hasConfig
  let
    (m,u)   = email_to         model
    subject = email_subject    model
    body    = emailWith (ServerAddress (view gc_backend_name cfg) (view gc_url cfg)) model
  liftBase $ gargMail mailCfg (GargMail { gm_to = m
                                        , gm_name = Just u
                                        , gm_subject = subject
                                        , gm_body = body })

------------------------------------------------------------------------
emailWith :: ServerAddress -> MailModel -> Text
emailWith server model =
  unlines $ [ "Hello" ]
          <> bodyWith server model
          <> email_disclaimer
          <> email_signature

------------------------------------------------------------------------
email_to :: MailModel -> (EmailAddress, Name)
email_to (Invitation user) = email_to' user
email_to (PassUpdate user) = email_to' user
email_to (MailInfo { .. })    = (mailInfo_address, mailInfo_username)
email_to (ForgotPassword { user = UserLight { .. }}) = (userLight_email, userLight_username)

email_to' :: NewUser GargPassword -> (EmailAddress, Name)
email_to' (NewUser u m _) = (m,u)

------------------------------------------------------------------------
bodyWith :: ServerAddress -> MailModel -> [Text]
bodyWith server@(ServerAddress name _url) (Invitation u) = [ "Congratulation, you have been granted a user account to test the"
                                 , "new GarganText platform called " <> name <> " !"
                                 ] <> (email_credentials server u)

bodyWith server (PassUpdate u) = [ "Your account password have been updated on the GarganText platform!"
                                 ] <> (email_credentials server u)

bodyWith (ServerAddress _ url) (MailInfo _ _) = [ "Your last analysis is over on the server: " <> url]
bodyWith _server (ForgotPassword { user = UserLight { userLight_forgot_password_uuid = Nothing }}) =
  [ "Cannot send you link to forgot password, no UUID" ]
bodyWith server (ForgotPassword { user = UserLight { userLight_forgot_password_uuid = Just uuid }}) =
  [ "Click on this link to restore your password: "
  , forgot_password_link server uuid ]

forgot_password_link :: ServerAddress -> Text -> Text
forgot_password_link (ServerAddress _ server) uuid = server <> "/#/forgotPassword?uuid=" <> uuid <> "&server=" <> encodeText server

------------------------------------------------------------------------
email_subject :: MailModel -> Text
email_subject (Invitation _)     = "[GarganText] Invitation"
email_subject (PassUpdate _)     = "[GarganText] Update"
email_subject (MailInfo _ _)     = "[GarganText] Info"
email_subject (ForgotPassword _) = "[GarganText] Forgot Password"


email_credentials :: ServerAddress -> NewUser GargPassword -> [Text]
email_credentials (ServerAddress _ server) (NewUser u _ (GargPassword p)) =
          [ ""
          , "You can log in to: " <> server
          , "Your username is: "  <> u
          , "Your password is: "  <> p
          , ""
          ]

email_disclaimer :: [Text]
email_disclaimer =
            [ ""
            , "/!\\ Please note that your account is opened for beta tester only. Hence"
            , "we cannot guarantee neither the perenniality nor the stability of the"
            , "service at this stage. It is therefore advisable to back up important"
            , "data regularly."
            , ""
            , "/!\\ Gargantext is an academic service supported by CNRS/ISC-PIF partners."
            , "In case of congestion on this service, access to members of the ISC-PIF"
            , "partners will be privileged."
            , ""
            , "If you log in you agree with the following terms of use:"
            , "     https://gitlab.iscpif.fr/humanities/tofu/tree/master"
            , ""
            , "Your feedback will be valuable for further development of the platform,"
            , "do not hesitate to contact us and to contribute on our forum:"
            , "     https://discourse.iscpif.fr/c/gargantext"
            , ""
            ]

email_signature :: [Text]
email_signature =
          [ "With our best regards,"
          , "-- "
          , "The Gargantext Team (CNRS)"
          ]
