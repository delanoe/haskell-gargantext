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

--import Gargantext.Config (dataPath)
import Data.Text (Text)
import GHC.IO (FilePath)
import Gargantext.Prelude
import System.Random (newStdGen)
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy.Char8  as Char
import qualified Data.Digest.Pure.SHA        as SHA (sha256, showDigest)
import qualified Data.Text                   as Text

type FolderPath = FilePath
type FileName   = FilePath

-- | TODO Env Monad
dataPath :: Text
dataPath = "data"

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

-- | Empreinte is a uniq sequence of Text to identify the Type
-- we want to save
type Empreinte = Text

saveFile :: SaveFile a => a -> IO FilePath
saveFile a = do
  let n = 3
  (fp,fn) <- (toPath n) . hash . Text.pack . show <$> newStdGen
  let foldPath = (Text.unpack dataPath) <> "/" <> fp
  let filePath = foldPath <> "/" <> fn
  _ <- createDirectoryIfMissing True foldPath
  _ <- saveFile' filePath a
  pure filePath

readFile :: ReadFile a => FilePath -> IO a
readFile fp = readFile' ((Text.unpack dataPath) <> "/" <> fp)

