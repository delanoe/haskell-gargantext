{-|
Module      : Main.hs
Description : Gargantext starter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Strict             #-}

module Main where

import Prelude (putStrLn)

import Options.Generic
import Data.Text (unpack)

import Gargantext.Prelude
import Gargantext.API (startGargantext, startGargantextMock)

--------------------------------------------------------
-- Tests
import qualified Gargantext.Graph.Utils                    as U
import qualified Gargantext.Graph.Distances.Conditional    as C
import qualified Gargantext.Graph.Distances.Distributional as D
import qualified Gargantext.Graph.Distances.Matrice        as M
--------------------------------------------------------

data Mode = Dev | Mock | Prod 
       deriving (Show, Read, Generic)
instance ParseRecord Mode
instance ParseField  Mode
instance ParseFields Mode


data MyOptions w = MyOptions { run  :: w ::: Mode        <?> "Possible modes: Dev | Mock | Prod"
                             , port :: w ::: Maybe Int   <?> "By default: 8008"
                             , ini  :: w ::: Maybe Text  <?> "Ini-file path of gargantext.ini"
                             }
          deriving (Generic)

instance ParseRecord (MyOptions Wrapped)
deriving instance Show (MyOptions Unwrapped)

main :: IO ()
main = do
  putStrLn $ show $ M.conditional $ M.myMat 10
  --putStrLn $ show $ M.size' $ M.myMat 100

main' :: IO ()
main' = do 
    MyOptions myMode myPort myIniFile  <- unwrapRecord
            "Gargantext: collaborative platform for text-mining"
    
    let myPort' = case myPort of
            Just p  -> p
            Nothing -> 8008

    let start = case myMode of
            --Nothing   -> startGargantext myPort' (unpack myIniFile')
            Prod -> startGargantext myPort' (unpack myIniFile')
                    where
                        myIniFile' = case myIniFile of
                                       Nothing -> panic "For Prod mode, you need to fill a gargantext.ini file"
                                       Just i  -> i
            Mock -> startGargantextMock myPort'
            _  -> startGargantextMock myPort'
    
    putStrLn $ "Starting Gargantext with mode: " <> show myMode
    start
