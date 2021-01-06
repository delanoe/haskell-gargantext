{-|
Module      : Gargantext.Core.Mail
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO put main configuration variables in gargantext.ini

-}

module Gargantext.Core.Mail
  where

import Data.Text (Text, unlines, splitOn)
import Gargantext.Core.Types.Individu
import Gargantext.Prelude
import Gargantext.Prelude.Mail (gargMail, GargMail(..))
import qualified Data.List as List

-- | Tool to put elsewhere
isEmail :: Text -> Bool
isEmail = ((==) 2) . List.length . (splitOn "@")

------------------------------------------------------------------------
data SendEmail    = SendEmail Bool

type EmailAddress  = Text
type Name          = Text
type ServerAddress = Text

data MailModel = Invitation { invitation_user :: NewUser GargPassword }
               | PassUpdate { passUpdate_user :: NewUser GargPassword }
               | MailInfo   { mailInfo_username :: Name
                            , mailInfo_address  :: EmailAddress
                            }
------------------------------------------------------------------------
------------------------------------------------------------------------
mail :: ServerAddress -> MailModel -> IO ()
mail server model = gargMail (GargMail m (Just u) subject body)
  where
    (m,u)   = email_to         model
    subject = email_subject    model
    body    = emailWith server model

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
email_to (MailInfo n m)    = (m, n)

email_to' :: NewUser GargPassword -> (EmailAddress, Name)
email_to' (NewUser u m _) = (u,m)

------------------------------------------------------------------------
bodyWith :: ServerAddress -> MailModel -> [Text]
bodyWith server (Invitation u) = [ "Congratulation, you have been granted a beta user account to test the"
                                 , "new GarganText platform!"
                                 ] <> (email_credentials server u)

bodyWith server (PassUpdate u) = [ "Your account password have been updated on the GarganText platform!"
                                 ] <> (email_credentials server u)

bodyWith server (MailInfo _ _) = [ "Your last analysis is over on the server: " <> server]

------------------------------------------------------------------------
email_subject :: MailModel -> Text
email_subject (Invitation _) = "[GarganText] Invitation"
email_subject (PassUpdate _) = "[GarganText] Update"
email_subject (MailInfo _ _) = "[GarganText] Info"


email_credentials :: ServerAddress -> NewUser GargPassword -> [Text]
email_credentials server (NewUser u _ (GargPassword p)) =
          [ ""
          , "You can log in to: " <> server
          , "Your username is: "  <> u
          , "Your password is: "  <> p
          , ""
          ]

email_disclaimer :: [Text]
email_disclaimer =
            [ ""
            , "If you log in you agree with the following terms of use:"
            , "          https://gitlab.iscpif.fr/humanities/tofu/tree/master"
            , ""
            , ""
            , "/!\\ Please note that your account is opened for beta tester only. Hence"
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

