module Main where

import Control.Monad
import Network.HTTP.Client
import Options.Generic
import Servant.Client

import Options
import Script (script)

main :: IO ()
main = do
  -- we parse CLI options
  opts@(ClientOpts (Helpful uri) _ _ (Helpful verb)) <- getRecord "Gargantext client"
  mgr <- newManager defaultManagerSettings
  burl <- parseBaseUrl uri
  when verb $ do
    putStrLn $ "[Debug] user: " ++ maybe "<none>" show (unHelpful $ user opts)
    putStrLn $ "[Debug] backend: " ++ show burl
  -- we run 'script' from the Script module, reporting potential errors
  res <- runClientM (script opts) (mkClientEnv mgr burl)
  case res of
    Left err -> putStrLn $ "[Client error] " ++ show err
    Right a  -> print a
