{-|
Module      : Main.hs
Description : Gargantext Admin tools
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX



 -}

{-# LANGUAGE Strict            #-}

module Main where

import Gargantext.API.Admin.Settings (withDevEnv, runCmdDev)
import Gargantext.API.Prelude (GargError)
import Gargantext.Database.Action.User.New (newUsers)
import Gargantext.Database.Prelude (Cmd'')
import Gargantext.Prelude
import System.Environment (getArgs)
import Gargantext.API.Admin.Types (DevEnv)

main :: IO ()
main = do
  (iniPath:mails) <- getArgs

  withDevEnv iniPath $ \env -> do
    x <- runCmdDev env ((newUsers $ map cs mails) :: Cmd'' DevEnv GargError Int64)
    putStrLn $ show x
  pure ()
