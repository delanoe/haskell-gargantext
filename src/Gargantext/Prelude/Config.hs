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
import Gargantext.Prelude
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Control.Lens (makeLenses)


data GargConfig = GargConfig { _gc_masteruser       :: !Text
                             , _gc_secretkey        :: !Text
                             , _gc_datafilepath     :: !FilePath

                             , _gc_frame_write_url  :: !Text
                             , _gc_frame_calc_url   :: !Text

                             , _gc_frame_searx_url  :: !Text
                             , _gc_frame_istex_url  :: !Text

                             , _gc_max_docs_scrapers :: !Integer
                             }
  deriving (Generic, Show)

makeLenses ''GargConfig

readConfig :: FilePath -> IO GargConfig
readConfig fp = do
  ini         <- readIniFile fp
  let ini'' = case ini of
        Left e     -> panic (pack $ "gargantext.ini not found" <> show e)
        Right ini' -> ini'

  let val x = case (lookupValue (pack "gargantext") (pack x) ini'') of
        Left _   -> panic (pack $ "ERROR: add " <> x <> " to your gargantext.ini")
        Right p' -> p'

  pure $ GargConfig (val "MASTER_USER")
                    (val "SECRET_KEY")
                    (cs $ val "DATA_FILEPATH")
                    (val "FRAME_WRITE_URL")
                    (val "FRAME_CALC_URL")
                    (val "FRAME_SEARX_URL")
                    (val "FRAME_ISTEX_URL")
                    (read $ cs $ val "MAX_DOCS_SCRAPERS")

defaultConfig :: GargConfig
defaultConfig = GargConfig "gargantua"
                           "secret"
                           "data"
                           "https://frame_write.url"
                           "https://frame_calc.url"
                           "https://frame_searx.url"
                           "https://frame_istex.url"
                           1000
