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
import Gargantext.Prelude
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Core.Mail
import Gargantext.Prelude.Config
import Gargantext.Database.Schema.User
import Gargantext.Database.Action.User
import Gargantext.Core.Types.Individu (User(..))

------------------------------------------------------------------------

sendMail :: HasNodeError err => User -> Cmd err ()
sendMail u = do
  server <- view $ hasConfig . gc_url
  userLight <- getUserLightDB u
  liftBase $ mail server (MailInfo { mailInfo_username = userLight_username userLight
                                   , mailInfo_address = userLight_email userLight })

