{-# LANGUAGE TypeOperators #-}
module Options where

import Options.Generic

-- | Some general options to be specified on the command line.
data ClientOpts = ClientOpts
  { url     :: String <?> "URL to gargantext backend"
  , user    :: Maybe Text <?> "(optional) username for auth-restricted actions"
  , pass    :: Maybe Text <?> "(optional) password for auth-restricted actions"
  , verbose :: Bool   <?> "Enable verbose output"
  } deriving (Generic, Show)

instance ParseRecord ClientOpts
