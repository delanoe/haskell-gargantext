{-|
Module      : Gargantext.Prelude.Utils
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Gargantext.Prelude.Utils
  where

import Control.Exception
import Control.Lens (view)
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad.Random.Class (MonadRandom)
import Data.Text (Text)
import qualified Data.Text             as Text
import GHC.IO (FilePath)
import System.Directory (createDirectoryIfMissing)
import qualified System.Directory as SD
import System.IO.Error
import System.Random (newStdGen)
import qualified System.Random.Shuffle as SRS

import Gargantext.API.Admin.Settings
import Gargantext.Config
import Gargantext.Core.Crypto.Hash
import Gargantext.Database.Admin.Types.Node (NodeId, NodeType)
import Gargantext.Prelude

--------------------------------------------------------------------------
shuffle :: MonadRandom m => [a] -> m [a]
shuffle ns = SRS.shuffleM ns 

--------------------------------------------------------------------------
data NodeToHash = NodeToHash { nodeType :: NodeType
                             , nodeId   :: NodeId
                             }

type FolderPath = FilePath
type FileName   = FilePath

-- | toPath example of use:
-- toPath 2 "gargantexthello"
-- ("ga/rg","antexthello")
-- 
-- toPath 3 "gargantexthello"
-- ("gar/gan","texthello")


toPath :: Int -> Text -> (FolderPath, FileName)
toPath n x = (Text.unpack $ Text.intercalate "/" [x1,x2], Text.unpack xs)
  where
    (x1,x') = Text.splitAt n x
    (x2,xs) = Text.splitAt n x'

class SaveFile a where
  saveFile' :: FilePath -> a -> IO ()

class ReadFile a where
  readFile' :: FilePath -> IO a


folderFilePath :: (MonadReader env m, MonadBase IO m) => m (FolderPath, FileName)
folderFilePath = do
  (foldPath, fileName)  <- liftBase $ (toPath 3) . hash . show <$> newStdGen

  pure (foldPath, fileName)


writeFile :: (MonadReader env m, MonadBase IO m, HasSettings env, SaveFile a)
          => a -> m FilePath
writeFile a = do
  dataPath <- view (settings . config . gc_datafilepath) <$> ask

  (foldPath, fileName) <- folderFilePath

  let filePath = foldPath <> "/" <> fileName
      dataFoldPath = dataPath <> "/" <> foldPath
      dataFileName = dataPath <> "/" <> filePath

  _ <- liftBase $ createDirectoryIfMissing True dataFoldPath
  _ <- liftBase $ saveFile' dataFileName a

  pure filePath


readFile :: (MonadReader env m, MonadBase IO m, HasSettings env, ReadFile a)
         => FilePath -> m a
readFile fp = do
  dataPath <- view (settings . config . gc_datafilepath) <$> ask
  liftBase $ readFile' $ dataPath <> "/" <> fp

removeFile :: (MonadReader env m, MonadBase IO m, HasSettings env)
           => FilePath -> m ()
removeFile fp = do
  dataPath <- view (settings . config . gc_datafilepath) <$> ask
  liftBase $ SD.removeFile (dataPath <> "/" <> fp) `catch` handleExists
    where
      handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
