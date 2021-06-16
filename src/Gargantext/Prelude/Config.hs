{-|
Module      : Gargantext.Prelude.Config
Description : Textmining Collaborative Platform
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.Prelude.Config where

import Prelude (read)
import System.IO (FilePath)
import Data.Ini (readIniFile, lookupValue)
import Data.Either.Extra (Either(Left, Right))
import Data.Text as T
import GHC.Generics (Generic)
import Control.Lens (makeLenses)

import Gargantext.Prelude


-- | strip a given character from end of string
stripRight :: Char -> T.Text -> T.Text
stripRight c s = if T.last s == c then stripRight c (T.take (T.length s - 1) s) else s

data GargConfig = GargConfig { _gc_url              :: !T.Text
                             , _gc_url_backend_api  :: !T.Text

                             , _gc_masteruser       :: !T.Text
                             , _gc_secretkey        :: !T.Text

                             , _gc_datafilepath     :: !FilePath
                             , _gc_repofilepath     :: !FilePath

                             , _gc_frame_write_url  :: !T.Text
                             , _gc_frame_calc_url   :: !T.Text
                             , _gc_frame_visio_url  :: !T.Text

                             , _gc_frame_searx_url  :: !T.Text
                             , _gc_frame_istex_url  :: !T.Text

                             , _gc_max_docs_scrapers :: !Integer
                             }
  deriving (Generic, Show)

makeLenses ''GargConfig

readConfig :: FilePath -> IO GargConfig
readConfig fp = do
  ini         <- readIniFile fp
  let ini'' = case ini of
        Left e     -> panic (T.pack $ "gargantext.ini not found" <> show e)
        Right ini' -> ini'

  let val x = case (lookupValue (T.pack "gargantext") (T.pack x) ini'') of
        Left _   -> panic (T.pack $ "ERROR: add " <> x <> " to your gargantext.ini")
        Right p' -> p'

  pure $ GargConfig (stripRight '/' $ val "URL")
                    (stripRight '/' $ val "URL_BACKEND_API")
                    (val "MASTER_USER")
                    (val "SECRET_KEY")
                    (cs $ val "DATA_FILEPATH")
                    (cs $ val "REPO_FILEPATH")
                    (stripRight '/' $ val "FRAME_WRITE_URL")
                    (stripRight '/' $ val "FRAME_CALC_URL")
                    (stripRight '/' $ val "FRAME_VISIO_URL")
                    (stripRight '/' $ val "FRAME_SEARX_URL")
                    (stripRight '/' $ val "FRAME_ISTEX_URL")
                    (read $ cs $ val "MAX_DOCS_SCRAPERS")

{- UNUSED
defaultConfig :: GargConfig
defaultConfig = GargConfig "https://localhost"
                           "https://localhost:8008/api/v1.0"
                           "gargantua"
                           "secret"
                           "data"
                           "repos/"
                           "https://frame_write.url"
                           "https://frame_calc.url"
                           "https://frame_searx.url"
                           "https://frame_istex.url"
                           1000
-}
