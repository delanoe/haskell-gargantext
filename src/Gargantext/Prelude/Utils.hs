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

import Prelude (String)
import Data.Set (Set)
import Data.List (foldl)
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
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import qualified System.Random.Shuffle as SRS

--------------------------------------------------------------------------
shuffle :: MonadRandom m => [a] -> m [a]
shuffle ns = SRS.shuffleM ns 

--------------------------------------------------------------------------
-- | Use this datatype to keep traceability of hashes
-- TODO use newtype
type Hash = Text

-- | Class to make hashes
class IsHashable a where
  hash :: a -> Hash

-- | Main API to hash text
-- using sha256 for now
instance IsHashable Char.ByteString where
  hash = Text.pack
        . SHA.showDigest
        . SHA.sha256

instance {-# OVERLAPPING #-} IsHashable String where
  hash = hash . Char.pack

instance IsHashable Text where
  hash = hash . Text.unpack

instance IsHashable (Set Hash) where
  hash = hash . foldl (<>) "" . Set.toList

instance {-# OVERLAPPABLE #-} IsHashable a => IsHashable [a] where
  hash = hash . Set.fromList . map hash

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
  (fp,fn)  <- liftBase $ (toPath 3) . hash . show <$> newStdGen

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
