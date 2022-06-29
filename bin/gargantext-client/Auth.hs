module Auth where

import Prelude
import Core
import Options

import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Options.Generic
import Servant.Client
import qualified Servant.Auth.Client as SA

import Gargantext.API.Client
import qualified Gargantext.API.Admin.Auth.Types as Auth
import qualified Gargantext.Core.Types.Individu  as Auth
import qualified Gargantext.Database.Admin.Types.Node as Node

-- | Authenticate and use the resulting Token to perform
--   auth-restricted actions
withAuthToken
  :: ClientOpts -- ^ source of user/pass data
  -> (SA.Token -> Node.NodeId -> ClientM a) -- ^ do something once authenticated
  -> ClientM a
withAuthToken opts act
  -- both user and password CLI arguments passed
  | Helpful (Just usr) <- user opts
  , Helpful (Just pw)  <- pass opts = do
      authRes <- postAuth (Auth.AuthRequest usr (Auth.GargPassword pw))
      case Auth._authRes_valid authRes of
        -- authentication failed, this function critically needs it to
        -- be able to run the action, so we abort
        Nothing -> problem $
          "invalid auth response: " ++
          maybe "" (show . Auth._authInv_message)
                   (Auth._authRes_inval authRes)
        -- authentication went through, we can run the action
        Just (Auth.AuthValid tok tree_id _uid) -> do
          let tok' = SA.Token (encodeUtf8 tok)
          whenVerbose opts $ do
            liftIO . putStrLn $ "[Debug] Authenticated: token=" ++ show tok ++
                                ", tree_id=" ++ show tree_id
          act tok' tree_id
  -- user and/or pass CLI arguments not passed
  | otherwise =
      problem "auth-protected actions require --user and --pass"
