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

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Gargantext.Database.Utils where

import Debug.Trace (trace)
import Prelude (String)
import Control.Monad ((>>))
import Data.Text (Text)
import Control.Monad.Error.Class (MonadError(..))
import Control.Lens (Getter, view)
import Control.Monad.Reader
import Control.Monad.Except
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
import Opaleye (Query, Unpackspec, showSqlForPostgres, FromFields, Select, runQuery)
import System.IO (FilePath)
import Text.Read (read)
import qualified Data.ByteString      as DB
import qualified Database.PostgreSQL.Simple as PGS

class HasConnection env where
  connection :: Getter env Connection

instance HasConnection Connection where
  connection = identity

type CmdM' env err m =
  ( MonadReader env m
  , MonadError err m
  , MonadIO m
  )

type CmdM env err m =
  ( CmdM' env err m
  , HasConnection env
  )

type Cmd' env err a = forall m. CmdM' env err m => m a

type Cmd err a = forall m env. CmdM env err m => m a

-- TODO: ideally there should be very few calls to this functions.
mkCmd :: (Connection -> IO a) -> Cmd err a
mkCmd k = do
  conn <- view connection
  liftIO $ k conn

runCmd :: HasConnection env => env
       -> Cmd' env err a
       -> IO (Either err a)
runCmd env m = runExceptT $ runReaderT m env

runOpaQuery :: Default FromFields fields haskells => Select fields -> Cmd err [haskells]
runOpaQuery q = mkCmd $ \c -> runQuery c q

formatPGSQuery :: PGS.ToRow a => PGS.Query -> a -> Cmd err DB.ByteString
formatPGSQuery q a = mkCmd $ \conn -> PGS.formatQuery conn q a

runPGSQuery' :: (PGS.ToRow a, PGS.FromRow b) => PGS.Query -> a -> Cmd err [b]
runPGSQuery' q a = mkCmd $ \conn -> PGS.query conn q a

runPGSQuery :: (MonadError err m, MonadReader env m,
                PGS.FromRow r, PGS.ToRow q, MonadIO m, HasConnection env)
                => PGS.Query -> q -> m [r]
runPGSQuery q a = catchError (mkCmd $ \conn -> PGS.query conn q a)
                              (\e ->  putStrLn ("Text xxxxxxxxxxxxxxxxxxx" :: String)
                              --(\e -> putStrLn ((cs $ formatPGSQuery q a):: Text)
                                   >> throwError e
                                )

execPGSQuery :: PGS.ToRow a => PGS.Query -> a -> Cmd err Int64
execPGSQuery q a = mkCmd $ \conn -> PGS.execute conn q a

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

fromField' :: (Typeable b, FromJSON b) => Field -> Maybe DB.ByteString -> Conversion b
fromField' field mb = do
    v <- fromField field mb
    valueToHyperdata v
      where
          valueToHyperdata v = case fromJSON v of
             Success a  -> pure a
             Error _err -> returnError ConversionFailed field "cannot parse hyperdata"

printSqlOpa :: Default Unpackspec a a => Query a -> IO ()
printSqlOpa = putStrLn . maybe "Empty query" identity . showSqlForPostgres

