{-|
Module      : Gargantext.Prelude.Utils
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Prelude.Utils
  where

import Control.Lens (view)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import GHC.IO (FilePath)
import Gargantext.API.Admin.Settings
import Gargantext.Database.Admin.Types.Node (NodeId, NodeType)
import Gargantext.Prelude
import System.Directory (createDirectoryIfMissing)
import System.Random (newStdGen)
import qualified Data.ByteString.Lazy.Char8  as Char
import qualified Data.Digest.Pure.SHA        as SHA (sha256, showDigest)
import qualified Data.Text                   as Text
import qualified System.Random.Shuffle as SRS

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

type FolderPath = FilePath
type FileName   = FilePath

toPath :: Int -> Text -> (FolderPath,FileName)
toPath n x = (Text.unpack $ Text.intercalate "/" [x1,x2], Text.unpack xs)
  where
    (x1,x') = Text.splitAt n x
    (x2,xs) = Text.splitAt n x'

class SaveFile a where
  saveFile' :: FilePath -> a -> IO ()

class ReadFile a where
  readFile' :: FilePath -> IO a


writeFile :: (MonadReader env m, MonadBase IO m, HasSettings env, SaveFile a)
         => a -> m FilePath
writeFile a = do
  dataPath <- view (settings . fileFolder) <$> ask
  (fp,fn)  <- liftBase $ (toPath 3) . sha . Text.pack . show <$> newStdGen
  
  let foldPath = dataPath <> "/" <> fp
      filePath = foldPath <> "/" <> fn
  
  _ <- liftBase $ createDirectoryIfMissing True foldPath
  _ <- liftBase $ saveFile' filePath a
  
  pure filePath


readFile :: (MonadReader env m, MonadBase IO m, HasSettings env, ReadFile a)
         => FilePath -> m a
readFile fp = do
  dataPath <- view (settings . fileFolder) <$> ask
  liftBase $ readFile' $ dataPath <> "/" <> fp
