{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Gargantext.Prelude
import Gargantext.Server (startGargantext)

import System.Environment (getArgs)

main :: IO ()
main = do 
  (iniFile:_) <- getArgs
  startGargantext iniFile
