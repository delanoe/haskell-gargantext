module Script (script) where

import Control.Monad.IO.Class
import Gargantext.API.Client
import Servant.Client

import Auth
import Core
import Options
import Tracking

-- | An example script. Tweak, rebuild and re-run the executable to see the
--   effect of your changes. You can hit any gargantext endpoint in the body
--   of 'script' using the many (many!) client functions exposed by the
--   'Gargantext.API.Client' module.
--
--   Don't forget to pass @--user@ and @--pass@ if you're using 'withAuthToken'.
script :: ClientOpts -> ClientM ()
script opts = do
  -- we start by asking the backend for its version
  ver <- getBackendVersion
  liftIO . putStrLn $ "Backend version: " ++ show ver

  -- next we authenticate using the credentials given on the command line
  -- (through --user and --pass), erroring out loudly if the auth creds don't
  -- go through, running the continuation otherwise.
  withAuthToken opts $ \tok userNode -> do
    liftIO . putStrLn $ "user node: " ++ show userNode
    steps <-
      -- we run a few client computations while tracking some EKG metrics
      -- (any RTS stats or routing-related data), which means that we sample the
      -- metrics at the beginning, the end, and in between each pair of steps.
      tracking opts ["rts.gc.bytes_allocated"]
        [ ("get roots", do
              roots <- getRoots tok
              liftIO . putStrLn $ "roots: " ++ show roots
          )
        , ("get user node detail", do
              userNodeDetail <- getNode tok userNode
              liftIO . putStrLn $ "user node details: " ++ show userNodeDetail
          )
        ]
    -- we pretty print the values we sampled for all metrics and the
    -- results of all the steps
    whenVerbose opts (ppTracked steps)
