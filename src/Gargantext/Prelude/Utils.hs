{-|
Module      : Gargantext.Prelude.Utils
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE     NoImplicitPrelude       #-}
{-# LANGUAGE     OverloadedStrings       #-}

module Gargantext.Prelude.Utils
  where

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Control.Monad.Reader (ask)
import GHC.IO (FilePath)
import Gargantext.Prelude
import Gargantext.API.Settings
import System.Random (newStdGen)
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy.Char8  as Char
import qualified Data.Digest.Pure.SHA        as SHA (sha256, showDigest)
import qualified Data.Text                   as Text

type FolderPath = FilePath
type FileName   = FilePath


hash :: Text -> Text
hash = Text.pack
     .  SHA.showDigest
     .  SHA.sha256
     . Char.pack
     . Text.unpack


toPath :: Int -> Text -> (FolderPath,FileName)
toPath n x = (Text.unpack $ Text.intercalate "/" [x1,x2], Text.unpack xs)
  where
    (x1,x') = Text.splitAt n x
    (x2,xs) = Text.splitAt n x'

class SaveFile a where
  saveFile' :: FilePath -> a -> IO ()

class ReadFile a where
  readFile' :: FilePath -> IO a


saveFile :: (MonadReader env m, MonadIO m, HasSettings env, SaveFile a)
         => a -> m FilePath
saveFile a = do
  dataPath <- view (settings . fileFolder) <$> ask
  (fp,fn)  <- liftIO $ (toPath 3) . hash . Text.pack . show <$> newStdGen
  
  let foldPath = dataPath <> "/" <> fp
      filePath = foldPath <> "/" <> fn
  
  _ <- liftIO $ createDirectoryIfMissing True foldPath
  _ <- liftIO $ saveFile' filePath a
  
  pure filePath


readFile :: (MonadReader env m, MonadIO m, HasSettings env, ReadFile a)
         => FilePath -> m a
readFile fp = do
  dataPath <- view (settings . fileFolder) <$> ask
  liftIO $ readFile' $ dataPath <> "/" <> fp
