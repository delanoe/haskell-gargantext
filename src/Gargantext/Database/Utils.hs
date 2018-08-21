{-|
Module      : Gargantext.Database.Util
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.Utils where

import qualified Database.PostgreSQL.Simple as PGS

import Data.Monoid ((<>))
import Data.Either.Extra (Either(Left, Right))
import Gargantext.Prelude
import Data.Text (unpack, pack)
import Text.Read (read)
import Data.Ini (readIniFile, lookupValue)
import Data.Word (Word16)
import System.IO (FilePath)
import Database.PostgreSQL.Simple (Connection, connect)

-- Utilities
import Opaleye (Query, Unpackspec, showSqlForPostgres)
import Data.Profunctor.Product.Default (Default)
import Data.Maybe (maybe)
-- TODO add a reader Monad here
-- read this in the init file

databaseParameters :: FilePath -> IO PGS.ConnectInfo
databaseParameters fp = do
  ini         <- readIniFile fp
  let ini'' = case ini of
        Left e     -> panic (pack $ "No ini file error" <> show e)
        Right ini' -> ini'

  let val x = case (lookupValue (pack "django") (pack x) ini'') of
        Left _ -> panic (pack $ "no" <> x)
        Right p' -> unpack p'

  pure $ PGS.ConnectInfo { PGS.connectHost     = val       "DB_HOST"
                         , PGS.connectPort     = read (val "DB_PORT") :: Word16
                         , PGS.connectUser     = val       "DB_USER"
                         , PGS.connectPassword = val       "DB_PASS"
                         , PGS.connectDatabase = val       "DB_NAME"
                         }

connectGargandb :: FilePath -> IO Connection
connectGargandb fp = databaseParameters fp >>= \params -> connect params

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . maybe "Empty query" identity . showSqlForPostgres

