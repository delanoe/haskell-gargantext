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
import Control.Monad.Random.Class (MonadRandom)
import Data.Text (Text)
import Control.Monad.Reader (ask)
import GHC.IO (FilePath)
import Gargantext.Prelude
import Gargantext.API.Settings
import System.Random (newStdGen)
import qualified System.Random.Shuffle as SRS
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy.Char8  as Char
import qualified Data.Digest.Pure.SHA        as SHA (sha256, showDigest)
import qualified Data.Text                   as Text
import Gargantext.Database.Types.Node (NodeId, NodeType)
import Data.ByteString (ByteString)
import Crypto.Argon2 as Crypto
import Data.Either
import Data.ByteString.Base64.URL as URL

--------------------------------------------------------------------------
shuffle :: MonadRandom m => [a] -> m [a]
shuffle ns = SRS.shuffleM ns 

--------------------------------------------------------------------------
sha :: Text -> Text
sha = Text.pack
     . SHA.showDigest
     . SHA.sha256
     . Char.pack
     . Text.unpack

--------------------------------------------------------------------------
data NodeToHash = NodeToHash { nodeType :: NodeType
                             , nodeId   :: NodeId
                             }

secret_key :: ByteString
secret_key = "WRV5ymit8s~ge6%08dLR7Q!gBcpb1MY%7e67db2206"

type SecretKey = ByteString

type FolderPath = FilePath
type FileName   = FilePath

hashNode :: SecretKey -> NodeToHash -> ByteString
hashNode sk (NodeToHash nt ni) = case hashResult of
    Left  e -> panic (cs $ show e)
    Right h -> URL.encode h
  where
    hashResult = Crypto.hash Crypto.defaultHashOptions
                  sk
                  (cs $ show nt <> show ni)


toPath :: Int -> Text -> (FolderPath,FileName)
toPath n x = (Text.unpack $ Text.intercalate "/" [x1,x2], Text.unpack xs)
  where
    (x1,x') = Text.splitAt n x
    (x2,xs) = Text.splitAt n x'

class SaveFile a where
  saveFile' :: FilePath -> a -> IO ()

class ReadFile a where
  readFile' :: FilePath -> IO a


writeFile :: (MonadReader env m, MonadIO m, HasSettings env, SaveFile a)
         => a -> m FilePath
writeFile a = do
  dataPath <- view (settings . fileFolder) <$> ask
  (fp,fn)  <- liftIO $ (toPath 3) . sha . Text.pack . show <$> newStdGen
  
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
