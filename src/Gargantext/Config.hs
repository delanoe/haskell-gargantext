{-|
Module      : Gargantext.Config
Description : Textmining Collaborative Platform
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.Config where

import System.IO (FilePath)
import Data.Ini (readIniFile, lookupValue)
import Data.Either.Extra (Either(Left, Right))
import Gargantext.Prelude
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Control.Lens (makeLenses)


data GargConfig = GargConfig { _gc_masteruser :: Text
                             , _gc_secretkey  :: Text
                             , _gc_frame_write_url :: Text
                             , _gc_frame_calc_url  :: Text
                             }
  deriving (Generic)

makeLenses ''GargConfig


readConfig :: FilePath -> IO GargConfig
readConfig fp = do
  ini         <- readIniFile fp
  let ini'' = case ini of
        Left e     -> panic (pack $ "No ini file error" <> show e)
        Right ini' -> ini'

  let val x = case (lookupValue (pack "gargantext") (pack x) ini'') of
        Left _   -> panic (pack $ "no" <> x)
        Right p' -> p'

  pure $ GargConfig (val "MASTER_USER")
                    (val "SECRET_KEY")
                    (val "FRAME_WRITE_URL")
                    (val "FRAME_CALC_URL")

defaultConfig :: GargConfig
defaultConfig = GargConfig "gargantua" "secret" "https://frame_write.url" "https://frame_calc.url"
