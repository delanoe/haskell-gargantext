-- |

module Gargantext.API.Dev where

import Gargantext.API.Admin.Settings
import Gargantext.API.Prelude
import Gargantext.API.Admin.Types
import Gargantext.Database.Prelude
import Gargantext.Prelude

-------------------------------------------------------------------
runCmdReplEasy :: Cmd'' DevEnv GargError a -> IO a
runCmdReplEasy f = withDevEnv "gargantext.ini" $ \env -> runCmdDev env f
