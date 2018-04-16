module Data.Gargantext.Error where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException)

import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
--import Text.Parsec.Error
--import Text.Parsec.Pos hiding (Line)


data GargError = GargIOError String IOError
               | GargHttpError String HttpException
               | GargParseError String
               | GargNgramsError String
               | GargDatabaseError String
               deriving (Show, Typeable, Generic)


instance Exception PandocError

-- | Handle GargError by exiting with an error message.
handleError :: Either GargError a -> IO a
handleError (Right r) = pure r
handleError (Left e) =
  case e of
    GargIOError _ err'   -> ioError err'
    GargHttpError u err' -> err 61 $
      "Could not fetch " ++ u ++ "\n" ++ show err'
    GargParseError s     -> err 64 s
    _ s                  -> err 0 s


err :: Int -> String -> IO a
err exitCode msg = do
  UTF8.hPutStrLn stderr msg
  exitWith $ ExitFailure exitCode
  return undefined
