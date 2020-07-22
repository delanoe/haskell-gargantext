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

module Gargantext.Core.Mail
  (gargMail)
  where

import Data.Maybe
import Network.Mail.SMTP hiding (htmlPart)
import Gargantext.Prelude
import Network.Mail.Mime (plainPart)


-- | TODO add parameters
gargMail :: IO ()
gargMail = sendMail "localhost" mail
  where
    mail = simpleMail from to cc bcc subject [body]

    from       = Address (Just "François Rabelais") "francois.rabelais@gargantext.org"
    to         = [Address (Just "Anoe") "alexandre@localhost"]
    cc         = []
    bcc        = []
    subject    = "email subject"
    body       = plainPart "email body"


