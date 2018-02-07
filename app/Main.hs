{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Gargantext.Prelude
import Gargantext.Server (startGargantext)
import Text.Read (read)
import System.Environment (getArgs)

main :: IO ()
main = do 
  (port:iniFile:_) <- getArgs
  startGargantext (read port :: Int) iniFile
