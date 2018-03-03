{-|
Module      : Main.hs
Description : Gargantext starter
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import Data.Text (unpack)

import Gargantext.Prelude
import Gargantext.API (startGargantext, startGargantextMock)

------------------------------------------------------

data Mode = Dev | Mock | Prod 
        deriving (Show, Read, Generic)
instance ParseRecord Mode
instance ParseField Mode
instance ParseFields Mode

data MyOptions = MyOptions { port    :: Maybe Int
                           , iniFile :: Maybe Text
                           , mode    :: Maybe Mode
                           } 
        deriving (Generic, Show)

instance ParseRecord MyOptions



main :: IO ()
main = do 
    MyOptions myPort myIniFile myMode <- getRecord 
            "Gargantext: collaborative platform for text-mining"
    
    let myPort' = case myPort of
            Just p  -> p
            Nothing -> 8008


    let start = case myMode of
            --Nothing   -> startGargantext myPort' (unpack myIniFile')
            Just Prod -> startGargantext myPort' (unpack myIniFile')
                    where
                        myIniFile' = case myIniFile of
                                       Nothing -> panic "Need gargantext.ini file"
                                       Just i  -> i
            Just Mock -> startGargantextMock myPort'
            _         -> startGargantextMock myPort'
    
    start
