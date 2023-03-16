{-|
Module      : Gargantext.Database.Action.Mail
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

module Gargantext.Database.Action.Mail
  where

import Control.Lens (view)
import Gargantext.Core.Mail (mail, MailModel(..))
import Gargantext.Core.Mail.Types (mailSettings)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.User
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Schema.User
import Gargantext.Prelude

------------------------------------------------------------------------

sendMail :: (HasNodeError err, CmdM env err m) => User -> m ()
sendMail u = do
  cfg       <- view $ mailSettings
  userLight <- getUserLightDB u
  mail cfg (MailInfo { mailInfo_username = userLight_username userLight
                     , mailInfo_address  = userLight_email    userLight
                     }
           )
