module Core (problem, whenVerbose) where

import Prelude

import Options
import Options.Generic

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Servant.Client

newtype GargClientException = GCE String
instance Show GargClientException where
  show (GCE s) = "Garg client exception: " ++ s

instance Exception GargClientException

-- | Abort with a message
problem :: String -> ClientM a
problem = throwM . GCE

-- | Only run the given computation when the @--verbose@ flag is
--   passed.
whenVerbose :: Monad m => ClientOpts -> m () -> m ()
whenVerbose opts act = when (unHelpful $ verbose opts) act
