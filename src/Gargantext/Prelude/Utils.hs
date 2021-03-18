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

import Data.Tuple.Extra (both)
import Control.Exception
import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Random.Class (MonadRandom)
import Data.Text (Text)
import qualified Data.Text             as Text
import GHC.IO (FilePath)
import System.Directory (createDirectoryIfMissing)
import qualified System.Directory as SD
import System.IO.Error
import System.Random (newStdGen)
import qualified System.Random.Shuffle as SRS

import Gargantext.Prelude.Config
import Gargantext.Prelude.Crypto.Hash
import Gargantext.Database.Prelude (HasConfig(..))
import Gargantext.Database.Admin.Types.Node (NodeId, NodeType)
import Gargantext.Prelude

--------------------------------------------------------------------------
shuffle :: MonadRandom m => [a] -> m [a]
shuffle ns = SRS.shuffleM ns 

--------------------------------------------------------------------------
data NodeToHash = NodeToHash { nodeType :: NodeType
                             , nodeId   :: NodeId
                             }
-------------------------------------------------------------------
type FolderPath = FilePath
type FileName   = FilePath

-- | toPath' example of use:
{-
>>> toPath' (1,2) ("","helloword")
("/he","lloword")

>>> toPath' (2,2) ("","helloword")
("/he/ll","oword")

>>> toPath' (2,3) ("","helloword")
("/hel/low","ord")
-}

toPath :: Int -> Text -> (FolderPath, FileName)
toPath n tx = both Text.unpack $ toPath' (2,n) ("", tx)

toPath' :: (Int,Int) -> (Text,Text) -> (Text,Text)
toPath' (n,m) (t,x) = foldl' (\tx _ -> toPath'' m tx) (t,x) [1..n]

toPath'' :: Int -> (Text, Text) -> (Text, Text)
toPath'' n (fp,fn) = (fp'',fn')
  where
    (fp',fn') = Text.splitAt n fn
    fp''       = Text.intercalate "/" [fp,fp']

-------------------------------------------------------------------
-------------------------------------------------------------------
class SaveFile a where
  saveFile' :: FilePath -> a -> IO ()

class ReadFile a where
  readFile' :: FilePath -> IO a


folderFilePath :: (MonadReader env m, MonadBase IO m) => m (FolderPath, FileName)
folderFilePath = do
  (foldPath, fileName)  <- liftBase $ (toPath 3) . hash . show <$> newStdGen

  pure (foldPath, fileName)


writeFile :: (MonadReader env m, MonadBase IO m, HasConfig env, SaveFile a)
          => a -> m FilePath
writeFile a = do
  dataPath <- view $ hasConfig . gc_datafilepath

  (foldPath, fileName) <- folderFilePath

  let filePath = foldPath <> "/" <> fileName
      dataFoldPath = dataPath <> "/" <> foldPath
      dataFileName = dataPath <> "/" <> filePath

  _ <- liftBase $ createDirectoryIfMissing True dataFoldPath
  _ <- liftBase $ saveFile' dataFileName a

  pure filePath


readFile :: (MonadReader env m, MonadBase IO m, HasConfig env, ReadFile a)
         => FilePath -> m a
readFile fp = do
  dataPath <- view $ hasConfig . gc_datafilepath
  liftBase $ readFile' $ dataPath <> "/" <> fp

removeFile :: (MonadReader env m, MonadBase IO m, HasConfig env)
           => FilePath -> m ()
removeFile fp = do
  dataPath <- view $ hasConfig . gc_datafilepath
  liftBase $ SD.removeFile (dataPath <> "/" <> fp) `catch` handleExists
    where
      handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
