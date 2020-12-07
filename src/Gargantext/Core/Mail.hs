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

------------------------------------------------------------------------
data SendEmail    = SendEmail Bool

type EmailAddress = Text
type ServerAdress = Text
data MailModel = Invitation
               | Update

------------------------------------------------------------------------
isEmail :: Text -> Bool
isEmail = ((==) 2) . List.length . (splitOn "@")
------------------------------------------------------------------------

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

