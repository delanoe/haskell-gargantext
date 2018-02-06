module Main where

--import System.Environment (getArgs)
import Data.Gargantext.Server (startGargantext)

main :: IO ()
--  (iniFile:_) <- getArgs
main = startGargantext -- port iniFile
