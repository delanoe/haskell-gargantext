{- NOTE This is legacy code. It keeps node stories in a directory
 repo. We now have migrated to the DB. However this code is needed to
 make the migration (see  Gargantext.API.Ngrams.Tools) -}

module Gargantext.Core.NodeStoryFile where

import Control.Lens (view)
import Control.Monad (foldM)
import Codec.Serialise (serialise, deserialise)
import Codec.Serialise.Class
import Control.Concurrent (MVar(), modifyMVar_, newMVar, readMVar, withMVar)
import Control.Debounce (mkDebounce, defaultDebounceSettings, debounceFreq, debounceAction)
import Gargantext.Core.NodeStory hiding (readNodeStoryEnv)
import Gargantext.Core.Types (ListId, NodeId(..))
import Gargantext.Database.Prelude (CmdM, hasConfig)
import Gargantext.Prelude
import Gargantext.Prelude.Config (gc_repofilepath)
import System.Directory (renameFile, createDirectoryIfMissing, doesFileExist, removeFile)
import System.IO (FilePath, hClose)
import System.IO.Temp (withTempFile)
import qualified Data.ByteString.Lazy                   as DBL
import qualified Data.List                              as List
import qualified Data.Map.Strict                        as Map
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams


getRepo :: HasNodeStory env err m
         => [ListId] -> m NodeListStory
getRepo listIds = do
  g <- getNodeListStory
  liftBase $ do
    v <- g listIds
    readMVar v
  -- v  <- liftBase $ f listIds
  -- v' <- liftBase $ readMVar v
  -- pure $ v'

getRepoReadConfig :: (CmdM env err m)
             => [ListId] -> m NodeListStory
getRepoReadConfig listIds = do
  repoFP <- view $ hasConfig . gc_repofilepath
  env <- liftBase $ readNodeStoryEnv repoFP
  let g = view nse_getter env
  liftBase $ do
    v <- g listIds
    readMVar v

getNodeListStory :: HasNodeStory env err m
                 => m ([NodeId] -> IO (MVar NodeListStory))
getNodeListStory = do
  env <- view hasNodeStory
  pure $ view nse_getter env



readNodeStoryEnv :: NodeStoryDir -> IO NodeStoryEnv
readNodeStoryEnv nsd = do
  mvar  <- nodeStoryVar nsd Nothing []
  saver <- mkNodeStorySaver nsd mvar
  let saver_immediate = withMVar mvar (writeNodeStories nsd)
  pure $ NodeStoryEnv { _nse_var = mvar
                      , _nse_saver = saver
                      , _nse_saver_immediate = saver_immediate
                      , _nse_getter = nodeStoryVar nsd (Just mvar) }

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
nodeStoryIncs _ Nothing    []        = pure $ NodeStory $ Map.empty
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
  _done <- mapM (writeNodeStory fp) $ splitByNode nls
  -- printDebug "[writeNodeStories]" done
  pure ()

writeNodeStory :: NodeStoryDir -> (NodeId, NodeListStory) -> IO ()
writeNodeStory rdfp (n, ns) = saverAction' rdfp n ns

splitByNode :: NodeListStory -> [(NodeId, NodeListStory)]
splitByNode (NodeStory m) =
  List.map (\(n,a) -> (n, NodeStory $ Map.singleton n a)) $ Map.toList m


saverAction' :: Serialise a => NodeStoryDir -> NodeId -> a -> IO ()
saverAction' repoDir nId a = do
  withTempFile repoDir ((cs $ show nId) <> "-tmp-repo.cbor") $ \fp h -> do
    -- printDebug "[repoSaverAction]" fp
    DBL.hPut h $ serialise a
    hClose h
    renameFile fp (nodeStoryPath repoDir nId)

nodeStoryPath :: NodeStoryDir -> NodeId -> FilePath
nodeStoryPath repoDir nId = repoDir <> "/" <> filename
  where
    filename = "repo" <> "-" <> (cs $ show nId) <> ".cbor"


------------------------------------------------------------------------
-- TODO : repo Migration TODO TESTS
{-
repoMigration :: NodeStoryDir -> NgramsRepo -> IO ()
repoMigration fp r = writeNodeStories fp (repoToNodeListStory r)

repoToNodeListStory :: NgramsRepo -> NodeListStory
repoToNodeListStory (Repo _v s h) = NodeStory $ Map.fromList ns
  where
    s' = ngramsState_migration      s
    h' = ngramsStatePatch_migration h
    ns = List.map (\(n,ns')
                    -> (n, let hs = fromMaybe [] (Map.lookup n h') in
                               Archive { _a_version = List.length hs
                                       , _a_state = ns'
                                       , _a_history = hs }
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
-}
