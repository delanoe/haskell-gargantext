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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gargantext.Database.Utils where

import Control.Applicative (Applicative)
import Control.Monad.Reader
import Data.Aeson (Result(Error,Success), fromJSON, FromJSON)
import Data.Either.Extra (Either(Left, Right))
import Data.Ini (readIniFile, lookupValue)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Profunctor.Product.Default (Default)
import Data.Text (unpack, pack)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Database.PostgreSQL.Simple (Connection, connect)
import Database.PostgreSQL.Simple.FromField ( Conversion, ResultError(ConversionFailed), fromField, returnError)
import Database.PostgreSQL.Simple.Internal  (Field)
import Gargantext.Prelude
import Opaleye (Query, Unpackspec, showSqlForPostgres)
import System.IO (FilePath)
import Text.Read (read)
import qualified Data.ByteString      as DB
import qualified Database.PostgreSQL.Simple as PGS

------------------------------------------------------------------------
{- | Reader Monad reinvented here:

newtype Cmd a = Cmd { unCmd :: Connection -> IO a }

instance Monad Cmd where
  return a = Cmd $ \_ -> return a

  m >>= f = Cmd $ \c -> do
    a <- unCmd m c
    unCmd (f a) c
-}

--type Cmd' a = forall m. (MonadReader env m, HasConnection env, MonadIO m) => m a

newtype Cmd a = Cmd (ReaderT Connection IO a)
  deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO)

runCmd :: Connection -> Cmd a -> IO a
runCmd c (Cmd f) = runReaderT f c

mkCmd :: (Connection -> IO a) -> Cmd a
mkCmd = Cmd . ReaderT

------------------------------------------------------------------------

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

fromField' :: (Typeable b, FromJSON b) => Field -> Maybe DB.ByteString -> Conversion b
fromField' field mb = do
    v <- fromField field mb
    valueToHyperdata v
      where
          valueToHyperdata v = case fromJSON v of
             Success a  -> pure a
             Error _err -> returnError ConversionFailed field "cannot parse hyperdata"

-- | Opaleye leftJoin* functions
-- TODO add here from Node.hs
