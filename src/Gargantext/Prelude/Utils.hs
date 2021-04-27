{-|
Module      : Gargantext.Prelude.Utils
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO_1: qualitative  tests (human)
TODO_2: quantitative tests (coded)


-}

module Gargantext.Prelude.Utils
  where

import Control.Exception
import Control.Lens (view)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (ToJSON, toJSON)
import Data.Text (Text)
import Data.Tuple.Extra (both)
import GHC.IO (FilePath)
import Gargantext.Database.Admin.Types.Node (NodeId, NodeType)
import Gargantext.Database.Prelude (HasConfig(..))
import Gargantext.Prelude
import Gargantext.Prelude.Config
import Gargantext.Prelude.Crypto.Hash
import System.Directory (createDirectoryIfMissing)
import System.IO.Error
import System.Random (newStdGen)
import qualified Data.Text             as Text
import qualified System.Directory      as SD
import qualified System.Random.Shuffle as SRS

-------------------------------------------------------------------
-- | Main Class to use (just declare needed functions)
class GargDB a where
  write :: a        -> IO ()
  read  :: FilePath -> IO a

  rm    :: (a, FilePath) -> IO ()
  mv    :: (a, FilePath) -> FilePath -> IO ()


-- | Why not this class too ?
class ToJSON parameters => GargDB' parameters gargdata where
  write' :: parameters -> gargdata -> IO ()
  read'  :: parameters -> IO gargdata

  rm'    :: gargdata -> parameters -> IO ()
  mv'    :: gargdata -> parameters -> parameters -> IO ()

-------------------------------------------------------------------
-- | Deprecated Class, use GargDB instead
class SaveFile a where
  saveFile' :: FilePath -> a -> IO ()

class ReadFile a where
  readFile' :: FilePath -> IO a

-------------------------------------------------------------------
-------------------------------------------------------------------
type GargFilePath = (FolderPath, FileName)
-- where
type FolderPath = FilePath
type FileName   = FilePath

           --------------------------------

dataFilePath :: (ToJSON a) => a -> GargFilePath
dataFilePath = toPath . hash . show . toJSON

randomFilePath :: ( MonadReader  env m
                  , MonadBase IO     m
                  )
               => m GargFilePath
randomFilePath = do
  (foldPath, fileName)  <- liftBase
                         $ toPath
                         . hash
                         . show
                         <$> newStdGen
  pure (foldPath, fileName)


-- | toPath' : how to hash text to path
{- example of use:
>>> toPath' (1,2) ("","helloword")
("/he","lloword")

>>> toPath' (2,2) ("","helloword")
("/he/ll","oword")

>>> toPath' (2,3) ("","helloword")
("/hel/low","ord")
-}
toPath :: Text -> (FolderPath, FileName)
toPath tx = both Text.unpack $ toPath' (2,3) ("", tx)

toPath' :: (Int,Int) -> (Text,Text) -> (Text,Text)
toPath' (n,m) (t,x) = foldl' (\tx _ -> toPath'' m tx) (t,x) [1..n]

toPath'' :: Int -> (Text, Text) -> (Text, Text)
toPath'' n (fp,fn) = (fp'',fn')
  where
    (fp',fn') = Text.splitAt n fn
    fp''      = Text.intercalate "/" [fp,fp']

-------------------------------------------------------------------
type DataPath   = FilePath
toFilePath :: FilePath -> FilePath -> FilePath
toFilePath fp1 fp2 = fp1 <> "/" <> fp2

-------------------------------------------------------------------

-- | Disk operations
-- | For example, this write file with a random filepath
-- better use a hash of json of Type used to parameter as input 
-- the functions
writeFile :: ( MonadReader env m
             , HasConfig   env
             , MonadBase IO    m
             , SaveFile        a
             )
          => a -> m FilePath
writeFile a = do
  dataPath <- view $ hasConfig . gc_datafilepath

  (foldPath, fileName) <- randomFilePath

  let filePath     = toFilePath foldPath fileName
      dataFoldPath = toFilePath dataPath foldPath
      dataFileName = toFilePath dataPath filePath

  _ <- liftBase $ createDirectoryIfMissing True dataFoldPath
  _ <- liftBase $ saveFile' dataFileName a

  pure filePath

---

-- | Example to read a file with Type 
readFile :: ( MonadReader  env m
            , HasConfig    env
            , MonadBase IO     m
            , ReadFile         a
            )
         => FilePath -> m a
readFile fp = do
  dataPath <- view $ hasConfig . gc_datafilepath
  liftBase $ readFile' $ toFilePath dataPath fp

---

rmFile :: ( MonadReader env m
              , MonadBase IO m
              , HasConfig env
              )
           => FilePath -> m ()
rmFile = onDisk_1 SD.removeFile

cpFile :: (MonadReader env m, MonadBase IO m, HasConfig env)
           => FilePath -> FilePath -> m ()
cpFile = onDisk_2 SD.copyFile

---

mvFile :: (MonadReader env m, MonadBase IO m, HasConfig env)
       => FilePath -> FilePath -> m ()
mvFile fp1 fp2 = do
  cpFile fp1 fp2
  rmFile fp1
  pure ()

------------------------------------------------------------------------
onDisk_1 :: ( MonadReader env m
              , MonadBase IO m
              , HasConfig env
              )
        => (FilePath -> IO ()) -> FilePath -> m ()
onDisk_1 action fp = do
  dataPath <- view $ hasConfig . gc_datafilepath
  liftBase $ action (toFilePath dataPath fp) `catch` handleExists
    where
      handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e


onDisk_2 :: ( MonadReader env m
              , MonadBase IO m
              , HasConfig env
              )
        => (FilePath -> FilePath -> IO ())
        -> FilePath
        -> FilePath
        -> m ()
onDisk_2 action fp1 fp2 = do
  dataPath <- view $ hasConfig . gc_datafilepath
  let fp1' = toFilePath dataPath fp1
      fp2' = toFilePath dataPath fp2
  liftBase $ action fp1' fp2' `catch` handleExists
    where
      handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e
------------------------------------------------------------------------

------------------------------------------------------------------------
-- | Misc Utils
shuffle :: MonadRandom m => [a] -> m [a]
shuffle ns = SRS.shuffleM ns 
--------------------------------------------------------------------------

-- TODO gargDB instance for NodeType
data NodeToHash = NodeToHash { nodeType :: NodeType
                             , nodeId   :: NodeId
                             }

