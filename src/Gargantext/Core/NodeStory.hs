{-|
Module      : Gargantext.Core.NodeStory
Description : Node API generation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO:
- remove
- filter
- charger les listes
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ConstraintKinds   #-}

module Gargantext.Core.NodeStory where

-- import Debug.Trace (traceShow)
import Codec.Serialise (serialise, deserialise)
import Codec.Serialise.Class 
import Control.Concurrent (MVar(), withMVar, newMVar, modifyMVar_)
import Control.Debounce (mkDebounce, defaultDebounceSettings, debounceFreq, debounceAction)
import Control.Lens (makeLenses, Getter, (^.))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson hiding ((.=), decode)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Semigroup
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (NodeId)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Prelude (CmdM', HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Prelude
import System.Directory (renameFile, createDirectoryIfMissing, doesFileExist, removeFile)
import System.IO (FilePath, hClose)
import System.IO.Temp (withTempFile)
import qualified Data.ByteString.Lazy                   as DBL
import qualified Data.List                              as List
import qualified Data.Map.Strict                        as Map
import qualified Data.Map.Strict.Patch.Internal         as Patch
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams

------------------------------------------------------------------------
data NodeStoryEnv = NodeStoryEnv
  { _nse_var    :: !(MVar NodeListStory)
  , _nse_saver  :: !(IO ())
  , _nse_getter :: [NodeId] -> IO (MVar NodeListStory)
  --, _nse_cleaner :: !(IO ()) -- every 12 hours: cleans the repos of unused NodeStories
  -- , _nse_lock  :: !FileLock -- TODO (it depends on the option: if with database or file only)
  }
  deriving (Generic)

type HasNodeStory env err m = ( CmdM' env err m
                              , MonadReader env m
                              , MonadError  err m
                              , HasNodeStoryEnv env
                              , HasConfig env
                              , HasConnectionPool env
                              , HasNodeError err
                              )

class (HasNodeStoryVar env, HasNodeStorySaver env)
  => HasNodeStoryEnv env where
    hasNodeStory :: Getter env NodeStoryEnv

class HasNodeStoryVar env where
  hasNodeStoryVar :: Getter env ([NodeId] -> IO (MVar NodeListStory))

class HasNodeStorySaver env where
  hasNodeStorySaver :: Getter env (IO ())

------------------------------------------------------------------------
readNodeStoryEnv :: NodeStoryDir -> IO NodeStoryEnv
readNodeStoryEnv nsd = do
  mvar  <- nodeStoryVar nsd Nothing [0]
  saver <- mkNodeStorySaver nsd mvar
  pure $ NodeStoryEnv mvar saver (nodeStoryVar nsd (Just mvar))

------------------------------------------------------------------------
mkNodeStorySaver :: NodeStoryDir -> MVar NodeListStory -> IO (IO ())
mkNodeStorySaver nsd mvns = mkDebounce settings
  where
    settings = defaultDebounceSettings
                 { debounceAction = withMVar mvns (writeNodeStories nsd)
                 , debounceFreq = 1 * minute
--                 , debounceEdge = trailingEdge -- Trigger on the trailing edge
                 }
    minute = 60 * second
    second = 10^(6 :: Int)

nodeStoryVar :: NodeStoryDir
             -> Maybe (MVar NodeListStory)
             -> [NodeId]
             -> IO (MVar NodeListStory)
nodeStoryVar nsd Nothing ni = nodeStoryIncs nsd Nothing ni >>= newMVar
nodeStoryVar nsd (Just mv) ni = do
  _ <- modifyMVar_ mv $ \mv' -> (nodeStoryIncs nsd (Just mv') ni)
  pure mv


nodeStoryInc :: NodeStoryDir -> Maybe NodeListStory -> NodeId -> IO NodeListStory
nodeStoryInc nsd (Just ns@(NodeStory nls)) ni = do
  case Map.lookup ni nls of
    Nothing -> do
      (NodeStory nls') <- nodeStoryRead nsd ni
      pure $ NodeStory $ Map.union nls nls'
    Just _  -> pure ns
nodeStoryInc nsd Nothing ni = nodeStoryRead nsd ni


nodeStoryIncs :: NodeStoryDir
              -> Maybe NodeListStory
              -> [NodeId]
              -> IO NodeListStory
nodeStoryIncs _ Nothing    []        = panic "nodeStoryIncs: Empty"
nodeStoryIncs nsd (Just nls) ns      = foldM (\m n -> nodeStoryInc nsd (Just m) n) nls ns
nodeStoryIncs nsd Nothing    (ni:ns) = do
  m <- nodeStoryRead nsd ni
  nodeStoryIncs nsd (Just m) ns


nodeStoryDec :: NodeStoryDir
             -> NodeListStory
             -> NodeId
             -> IO NodeListStory
nodeStoryDec nsd ns@(NodeStory nls) ni = do
  case Map.lookup ni nls of
    Nothing -> do
      -- we make sure the corresponding file repo is really removed
      _ <- nodeStoryRemove nsd ni
      pure ns
    Just _  -> do
      let ns' = Map.filterWithKey (\k _v -> k /= ni) nls
      _ <- nodeStoryRemove nsd ni
      pure $ NodeStory ns'

-- | TODO lock
nodeStoryRead :: NodeStoryDir -> NodeId -> IO NodeListStory
nodeStoryRead nsd ni = do
  _repoDir <- createDirectoryIfMissing True nsd
  let nsp = nodeStoryPath nsd ni
  exists <- doesFileExist nsp
  if exists
     then deserialise <$> DBL.readFile nsp
     else pure (initNodeStory ni)

nodeStoryRemove :: NodeStoryDir -> NodeId -> IO ()
nodeStoryRemove nsd ni = do
  let nsp = nodeStoryPath nsd ni
  exists <- doesFileExist nsp
  if exists
     then removeFile nsp
     else pure ()



nodeStoryRead_test :: NodeStoryDir -> NodeId -> IO (Maybe [ TableNgrams.NgramsType ])
nodeStoryRead_test nsd ni = nodeStoryRead nsd ni >>= \n -> pure
                          $ fmap Map.keys
                          $ fmap _a_state
                          $ Map.lookup ni
                          $ _unNodeStory n

------------------------------------------------------------------------
type NodeStoryDir = FilePath

writeNodeStories :: NodeStoryDir -> NodeListStory -> IO ()
writeNodeStories fp nls = do
  done <- mapM (writeNodeStory fp) $ splitByNode nls
  printDebug "[writeNodeStories]" done
  pure ()

writeNodeStory :: NodeStoryDir -> (NodeId, NodeListStory) -> IO ()
writeNodeStory rdfp (n, ns) = saverAction' rdfp n ns

splitByNode :: NodeListStory -> [(NodeId, NodeListStory)]
splitByNode (NodeStory m) =
  List.map (\(n,a) -> (n, NodeStory $ Map.singleton n a)) $ Map.toList m


saverAction' :: Serialise a => NodeStoryDir -> NodeId -> a -> IO ()
saverAction' repoDir nId a = do
  withTempFile repoDir ((cs $ show nId) <> "-tmp-repo.cbor") $ \fp h -> do
    printDebug "[repoSaverAction]" fp
    DBL.hPut h $ serialise a
    hClose h
    renameFile fp (nodeStoryPath repoDir nId)

nodeStoryPath :: NodeStoryDir -> NodeId -> FilePath
nodeStoryPath repoDir nId = repoDir <> "/" <> filename
  where
    filename = "repo" <> "-" <> (cs $ show nId) <> ".cbor"


------------------------------------------------------------------------
-- TODO : repo Migration TODO TESTS
repoMigration :: NodeStoryDir -> NgramsRepo -> IO ()
repoMigration fp r = writeNodeStories fp (repoToNodeListStory r)

repoToNodeListStory :: NgramsRepo -> NodeListStory
repoToNodeListStory (Repo _v s h) = NodeStory $ Map.fromList ns
  where
    s' = ngramsState_migration      s
    h' = ngramsStatePatch_migration h
    ns = List.map (\(n,ns')
                    -> (n, let hs = fromMaybe [] (Map.lookup n h') in
                               Archive (List.length hs) ns' hs
                       )
                  ) $ Map.toList s'

ngramsState_migration :: NgramsState
                      -> Map NodeId NgramsState'
ngramsState_migration ns =
  Map.fromListWith (Map.union) $ 
  List.concat $
    map (\(nt, nTable)
          -> map (\(nid, table)
                   -> (nid, Map.singleton nt table)
                 ) $ Map.toList nTable
        ) $ Map.toList ns


ngramsStatePatch_migration :: [NgramsStatePatch]
                           -> Map NodeId [NgramsStatePatch']
ngramsStatePatch_migration np' = Map.fromListWith (<>)
                               $ List.concat
                               $ map toPatch np'
  where
    toPatch :: NgramsStatePatch -> [(NodeId, [NgramsStatePatch'])]
    toPatch p = 
      List.concat $
        map (\(nt, nTable)
              -> map (\(nid, table)
                       -> (nid, [fst $ Patch.singleton nt table])
                     ) $ Patch.toList nTable
            ) $ Patch.toList p

------------------------------------------------------------------------

{- | Node Story for each NodeType where the Key of the Map is NodeId
  TODO : generalize for any NodeType, let's start with NodeList which
  is implemented already
-}
data NodeStory s p = NodeStory { _unNodeStory :: Map NodeId (Archive s p) }
  deriving (Generic, Show)

instance (FromJSON s, FromJSON p) => FromJSON (NodeStory s p)
instance (ToJSON s, ToJSON p) => ToJSON (NodeStory s p)
instance (Serialise s, Serialise p) => Serialise (NodeStory s p)

data Archive s p = Archive
  { _a_version :: !Version
  , _a_state   :: !s
  , _a_history :: ![p]
    -- first patch in the list is the most recent
  }
  deriving (Generic, Show)

instance (Serialise s, Serialise p) => Serialise (Archive s p)


type NodeListStory     = NodeStory NgramsState' NgramsStatePatch'

type NgramsState'      = Map       TableNgrams.NgramsType NgramsTableMap
type NgramsStatePatch' = PatchMap  TableNgrams.NgramsType NgramsTablePatch
instance Serialise NgramsStatePatch'

-- TODO Semigroup instance for unions
-- TODO check this
instance (Semigroup s, Semigroup p) => Semigroup (Archive s p) where
  (<>) (Archive _v _s p) (Archive v' s' p') = Archive v' s' (p' <> p)

instance Monoid (Archive NgramsState' NgramsStatePatch') where
  mempty = Archive 0 mempty []

instance (FromJSON s, FromJSON p) => FromJSON (Archive s p) where
  parseJSON = genericParseJSON $ unPrefix "_a_"

instance (ToJSON s, ToJSON p) => ToJSON (Archive s p) where
  toJSON     = genericToJSON     $ unPrefix "_a_"
  toEncoding = genericToEncoding $ unPrefix "_a_"

------------------------------------------------------------------------
initNodeStory :: Monoid s => NodeId -> NodeStory s p
initNodeStory ni = NodeStory $ Map.singleton ni initArchive

initArchive :: Monoid s => Archive s p
initArchive = Archive 0 mempty []

initNodeListStoryMock :: NodeListStory
initNodeListStoryMock = NodeStory $ Map.singleton nodeListId archive
  where
    nodeListId = 0
    archive        = Archive 0 ngramsTableMap []
    ngramsTableMap = Map.singleton TableNgrams.NgramsTerms
                   $ Map.fromList
                   [ (n ^. ne_ngrams, ngramsElementToRepo n)
                   | n <- mockTable ^. _NgramsTable
                   ]

------------------------------------------------------------------------


------------------------------------------------------------------------
-- | Lenses at the bottom of the file because Template Haskell would reorder order of execution in others cases
makeLenses ''NodeStoryEnv
makeLenses ''NodeStory
makeLenses ''Archive
