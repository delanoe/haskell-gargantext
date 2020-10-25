{-|
Module      : Gargantext.Core.Mail
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Prelude.Mail
  (gargMail, GargMail(..))
  where

-- import Data.Text.Internal.Lazy (Text)
import Data.Text (Text)
import Data.Maybe
import Network.Mail.SMTP hiding (htmlPart)
import Gargantext.Prelude
import Network.Mail.Mime (plainPart)


type Email = Text
type Name  = Text

data GargMail = GargMail { gm_to      :: Email
                         , gm_name    :: Maybe Name
                         , gm_subject :: Text
                         , gm_body    :: Text
                         }

-- | TODO add parameters to gargantext.ini
gargMail :: GargMail -> IO ()
gargMail (GargMail to' name subject body) = sendMail "localhost" mail
  where
    mail = simpleMail from to cc bcc subject [plainPart $ cs body]

    from       = Address (Just "GargTeam") "contact@gargantext.org"
    to         = [Address name to']
    cc         = []
    bcc        = []


