{-|
Module      : Gargantext.Database.Prelude
Description : Specific Prelude for Database management
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds   #-}

module Gargantext.Database.Prelude where

import Control.Exception
import Control.Lens (Getter, view)
import Control.Monad.Error.Class -- (MonadError(..), Error)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (Result(Error,Success), fromJSON, FromJSON)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Either.Extra (Either(Left, Right))
import Data.Ini (readIniFile, lookupValue)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.Default (Default)
import Data.Text (unpack, pack)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Database.PostgreSQL.Simple (Connection, connect)
import Database.PostgreSQL.Simple.FromField ( Conversion, ResultError(ConversionFailed), fromField, returnError)
import Database.PostgreSQL.Simple.Internal  (Field)
import Gargantext.Prelude
import Gargantext.Prelude.Config (GargConfig())
import Opaleye (Query, Unpackspec, showSqlForPostgres, FromFields, Select, runQuery)
import Opaleye.Aggregate (countRows)
import System.IO (FilePath)
import System.IO (stderr)
import Text.Read (read)
import qualified Data.ByteString      as DB
import qualified Data.List as DL
import qualified Database.PostgreSQL.Simple as PGS

-------------------------------------------------------
class HasConnectionPool env where
  connPool :: Getter env (Pool Connection)

instance HasConnectionPool (Pool Connection) where
  connPool = identity

class HasConfig env where
  hasConfig :: Getter env GargConfig

instance HasConfig GargConfig where
  hasConfig = identity

-------------------------------------------------------
type CmdM' env err m =
  ( MonadReader env m
  , MonadError err m
  , MonadBaseControl IO m
  )

type CmdM env err m =
  ( CmdM' env err m
  , HasConnectionPool env
  , HasConfig         env
  )

type Cmd' env err a = forall m. CmdM' env err m => m a

type Cmd err a = forall m env. CmdM env err m => m a

fromInt64ToInt :: Int64 -> Int
fromInt64ToInt = fromIntegral

-- TODO: ideally there should be very few calls to this functions.
mkCmd :: (Connection -> IO a) -> Cmd err a
mkCmd k = do
  pool <- view connPool
  withResource pool (liftBase . k)

runCmd :: (HasConnectionPool env)
       => env
       -> Cmd' env err a
       -> IO (Either err a)
runCmd env m = runExceptT $ runReaderT m env

runOpaQuery :: Default FromFields fields haskells
            => Select fields
            -> Cmd err [haskells]
runOpaQuery q = mkCmd $ \c -> runQuery c q

runCountOpaQuery :: Select a -> Cmd err Int
runCountOpaQuery q = do
  counts <- mkCmd $ \c -> runQuery c $ countRows q
  -- countRows is guaranteed to return a list with exactly one row so DL.head is safe here
  pure $ fromInt64ToInt $ DL.head counts

formatPGSQuery :: PGS.ToRow a => PGS.Query -> a -> Cmd err DB.ByteString
formatPGSQuery q a = mkCmd $ \conn -> PGS.formatQuery conn q a

-- TODO use runPGSQueryDebug everywhere
runPGSQuery' :: (PGS.ToRow a, PGS.FromRow b) => PGS.Query -> a -> Cmd err [b]
runPGSQuery' q a = mkCmd $ \conn -> PGS.query conn q a

runPGSQuery :: (MonadError err m, MonadReader env m, MonadBaseControl IO m,
                PGS.FromRow r, PGS.ToRow q, HasConnectionPool env, HasConfig env)
                => PGS.Query -> q -> m [r]
runPGSQuery q a = mkCmd $ \conn -> catch (PGS.query conn q a) (printError conn)
  where
    printError c (SomeException e) = do
      q' <- PGS.formatQuery c q a
      hPutStrLn stderr q'
      throw (SomeException e)


execPGSQuery :: PGS.ToRow a => PGS.Query -> a -> Cmd err Int64
execPGSQuery q a = mkCmd $ \conn -> PGS.execute conn q a

------------------------------------------------------------------------

databaseParameters :: FilePath -> IO PGS.ConnectInfo
databaseParameters fp = do
  ini         <- readIniFile fp
  let ini'' = case ini of
        Left e     -> panic (pack $ "No ini file error" <> show e)
        Right ini' -> ini'

  let val x = case (lookupValue (pack "database") (pack x) ini'') of
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
             Error _err -> returnError ConversionFailed field
                         $ DL.intercalate " " [ "cannot parse hyperdata for JSON: "
                                              , show v
                                              ]

printSqlOpa :: Default Unpackspec a a => Query a -> IO ()
printSqlOpa = putStrLn . maybe "Empty query" identity . showSqlForPostgres

