{-|
Module      : Gargantext.Core.Mail.Types
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Mail.Types where

import Control.Lens (Getter)
import Gargantext.Prelude.Mail.Types (MailConfig)

class HasMail env where
  mailSettings :: Getter env MailConfig
