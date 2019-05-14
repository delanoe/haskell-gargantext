{-|
Module      : Main.hs
Description : Gargantext starter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Script to start gargantext with different modes (Dev, Prod, Mock).

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE Strict             #-}

module Main where


import Options.Generic
import Data.Text (unpack)

import Gargantext.Prelude
import Gargantext.API (startGargantext) -- , startGargantextMock)

--------------------------------------------------------
-- Graph Tests
--import qualified Gargantext.Graph.Utils                    as U
--import qualified Gargantext.Graph.Distances.Conditional    as C
--import qualified Gargantext.Graph.Distances.Distributional as D
--import qualified Gargantext.Graph.Distances.Matrice        as M
--------------------------------------------------------

data Mode = Dev | Mock | Prod 
       deriving (Show, Read, Generic)
instance ParseRecord Mode
instance ParseField  Mode
instance ParseFields Mode


data MyOptions w =
  MyOptions { run  :: w ::: Mode
                        <?> "Possible modes: Dev | Mock | Prod"
            , port :: w ::: Maybe Int
                        <?> "By default: 8008"
            , ini  :: w ::: Maybe Text
                        <?> "Ini-file path of gargantext.ini"
            }
   deriving (Generic)

instance ParseRecord (MyOptions Wrapped)
deriving instance Show (MyOptions Unwrapped)


main :: IO ()
main = do
  MyOptions myMode myPort myIniFile  <- unwrapRecord
          "Gargantext server"

  let myPort' = case myPort of
        Just p  -> p
        Nothing -> 8008

  let start = case myMode of
        Prod -> startGargantext myPort' (unpack myIniFile')
            where
              myIniFile' = case myIniFile of
                  Nothing -> panic "[ERROR] gargantext.ini needed"
                  Just i  -> i
        Dev -> panic "[ERROR] Dev mode unsupported"
        Mock -> panic "[ERROR] Mock mode unsupported"
        -- _ -> startGargantextMock myPort'

  putStrLn $ "Starting with " <> show myMode <> " mode."
  start

