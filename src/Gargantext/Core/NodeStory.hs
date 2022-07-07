{-|
Module      : Gargantext.Core.NodeStory
Description : Node API generation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

A Node Story is a Map between NodeId and an Archive (with state,
version and history) for that node.

TODO:
- remove
- filter
- charger les listes
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.NodeStory
  ( HasNodeStory
  , HasNodeStoryEnv
  , hasNodeStory
  , HasNodeStoryVar
  , hasNodeStoryVar
  , HasNodeStorySaver
  , hasNodeStorySaver
  , NodeStory(..)
  , NgramsStatePatch'
  , NodeListStory
  , initNodeListStoryMock
  , NodeStoryEnv(..)
  , initNodeStory
  , nse_getter
  , nse_saver
  , nse_var
  , unNodeStory
  , Archive(..)
  , initArchive
  , a_history
  , a_state
  , a_version
  , nodeExists
  , getNodesIdWithType
  , readNodeStoryEnv
  , upsertNodeArchive
  , getNodeStory )
where

-- import Debug.Trace (traceShow)
import Control.Debounce (mkDebounce, defaultDebounceSettings, debounceFreq, debounceAction)
import Codec.Serialise.Class 
import Control.Arrow (returnA)
import Control.Concurrent (MVar(), withMVar, newMVar, modifyMVar_)
import Control.Exception (catch, throw, SomeException(..))
import Control.Lens (makeLenses, Getter, (^.))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson hiding ((.=), decode)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Pool (Pool, withResource)
import Data.Semigroup
import qualified Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.FromField (FromField(fromField), fromJSONField)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (NodeId(..), NodeType)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Prelude (CmdM', HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Prelude
import Opaleye (Column, DefaultFromField(..), Insert(..), Select, SqlInt4, SqlJsonb, Table, Update(..), (.==), fromPGSFromField, rCount, restrict, runInsert, runSelect, runUpdate, selectTable, sqlInt4, sqlValueJSONB, tableField, updateEasy)
import Opaleye.Internal.Table (Table(..))
import System.IO (stderr)
import qualified Data.Map.Strict                        as Map
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
instance FromField (Archive NgramsState' NgramsStatePatch')
  where
    fromField = fromJSONField
instance DefaultFromField SqlJsonb (Archive NgramsState' NgramsStatePatch')
  where
    defaultFromField = fromPGSFromField

-- TODO Semigroup instance for unions
-- TODO check this
instance (Semigroup s, Semigroup p) => Semigroup (Archive s p) where
  (<>) (Archive { _a_history = p }) (Archive { _a_version = v'
                                             , _a_state = s'
                                             , _a_history = p'}) =
    Archive { _a_version = v'
            , _a_state = s'
            , _a_history = p' <> p }

instance Monoid (Archive NgramsState' NgramsStatePatch') where
  mempty = Archive { _a_version = 0
                   , _a_state = mempty
                   , _a_history = [] }

instance (FromJSON s, FromJSON p) => FromJSON (Archive s p) where
  parseJSON = genericParseJSON $ unPrefix "_a_"

instance (ToJSON s, ToJSON p) => ToJSON (Archive s p) where
  toJSON     = genericToJSON     $ unPrefix "_a_"
  toEncoding = genericToEncoding $ unPrefix "_a_"

------------------------------------------------------------------------
initNodeStory :: Monoid s => NodeId -> NodeStory s p
initNodeStory ni = NodeStory $ Map.singleton ni initArchive

initArchive :: Monoid s => Archive s p
initArchive = Archive { _a_version = 0
                      , _a_state = mempty
                      , _a_history = [] }

initNodeListStoryMock :: NodeListStory
initNodeListStoryMock = NodeStory $ Map.singleton nodeListId archive
  where
    nodeListId = 0
    archive        = Archive { _a_version = 0
                             , _a_state = ngramsTableMap
                             , _a_history = [] }
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

-----------------------------------------


data NodeStoryPoly a b = NodeStoryDB { node_id :: a
                                     , archive :: b }
  deriving (Eq)

type ArchiveQ = Archive NgramsState' NgramsStatePatch'

type NodeStoryWrite = NodeStoryPoly (Column SqlInt4) (Column SqlJsonb)
type NodeStoryRead = NodeStoryPoly (Column SqlInt4) (Column SqlJsonb)

$(makeAdaptorAndInstance "pNodeStory" ''NodeStoryPoly)


runPGSQuery :: (PGS.FromRow r, PGS.ToRow q) => Pool PGS.Connection -> PGS.Query -> q -> IO [r]
runPGSQuery pool q a = withResource pool $ \c -> catch (PGS.query c q a) (printError c)
  where    
    printError c (SomeException e) = do
      q' <- PGS.formatQuery c q a
      hPutStrLn stderr q'
      throw (SomeException e)


nodeExists :: Pool PGS.Connection -> NodeId -> IO Bool
nodeExists pool nId = (== [PGS.Only True])
  <$> runPGSQuery pool [sql|SELECT true FROM nodes WHERE id = ? AND ? |] (nId, True)

getNodesIdWithType :: Pool PGS.Connection -> NodeType -> IO [NodeId]
getNodesIdWithType pool nt = do
  --ns <- withResource pool $ \c -> runSelect c $ selectNodesIdWithType nt
  ns <- runPGSQuery pool query (nodeTypeId nt, True)
  pure $ map (\(PGS.Only nId) -> NodeId nId) ns
  --pure (map NodeId ns)
  where
    query :: PGS.Query
    query = [sql|SELECT id FROM nodes WHERE typename = ? AND ? |]



nodeStoryTable :: Table NodeStoryRead NodeStoryWrite
nodeStoryTable =
  Table "node_stories"
    ( pNodeStory NodeStoryDB { node_id = tableField "node_id"
                             , archive = tableField "archive" } )
  
nodeStorySelect :: Select NodeStoryRead
nodeStorySelect = selectTable nodeStoryTable

getNodeStory :: Pool PGS.Connection -> NodeId -> IO NodeListStory
getNodeStory pool (NodeId nodeId) = do
  res <- withResource pool $ \c -> runSelect c query
  pure $ NodeStory $ Map.fromListWith (<>) $ (\(NodeStoryDB nId a) -> (nId, a)) <$> res
  where
    query :: Select NodeStoryRead
    query = proc () -> do
      row@(NodeStoryDB node_id _) <- nodeStorySelect -< ()
      restrict -< node_id .== sqlInt4 nodeId
      returnA -< row

insertNodeArchive :: Pool PGS.Connection -> NodeId -> ArchiveQ -> IO Int64
insertNodeArchive pool (NodeId nId) a = withResource pool $ \c -> runInsert c insert
  where
    insert = Insert { iTable      = nodeStoryTable
                    , iRows       = [NodeStoryDB { node_id = sqlInt4 nId
                                                 , archive = sqlValueJSONB a }]
                    , iReturning  = rCount
                    , iOnConflict = Nothing }

updateNodeArchive :: Pool PGS.Connection -> NodeId -> ArchiveQ -> IO Int64
updateNodeArchive pool (NodeId nId) a = withResource pool $ \c -> runUpdate c update
  where
    update = Update { uTable      = nodeStoryTable
                    , uUpdateWith = updateEasy (\(NodeStoryDB { .. }) -> NodeStoryDB { archive = sqlValueJSONB a, .. })
                    , uWhere      = (\row -> node_id row .== sqlInt4 nId)
                    , uReturning  = rCount }

-- nodeStoryRemove :: Pool PGS.Connection -> NodeId -> IO Int64
-- nodeStoryRemove pool (NodeId nId) = withResource pool $ \c -> runDelete c delete
--   where
--     delete = Delete { dTable     = nodeStoryTable
--                     , dWhere     = (\row -> node_id row .== sqlInt4 nId)
--                     , dReturning = rCount }

upsertNodeArchive :: Pool PGS.Connection -> NodeId -> ArchiveQ -> IO Int64
upsertNodeArchive pool nId a = do
  (NodeStory m) <- getNodeStory pool nId
  case Map.lookup nId m of
    Nothing -> insertNodeArchive pool nId a
    Just _  -> updateNodeArchive pool nId a

writeNodeStories :: Pool PGS.Connection -> NodeListStory -> IO ()
writeNodeStories pool (NodeStory nls) = do
  _ <- mapM (\(nId, a) -> upsertNodeArchive pool nId a) $ Map.toList nls
  pure ()
  
-- | Returns a `NodeListStory`, updating the given one for given `NodeId`
nodeStoryInc :: Pool PGS.Connection -> Maybe NodeListStory -> NodeId -> IO NodeListStory
nodeStoryInc pool Nothing nId = getNodeStory pool nId
nodeStoryInc pool (Just ns@(NodeStory nls)) nId = do
  case Map.lookup nId nls of
    Nothing -> do
      (NodeStory nls') <- getNodeStory pool nId
      pure $ NodeStory $ Map.union nls nls'
    Just _ -> pure ns

nodeStoryIncs :: Pool PGS.Connection -> Maybe NodeListStory -> [NodeId] -> IO NodeListStory
nodeStoryIncs _ Nothing [] = pure $ NodeStory $ Map.empty
nodeStoryIncs pool (Just nls) ns = foldM (\m n -> nodeStoryInc pool (Just m) n) nls ns
nodeStoryIncs pool Nothing (ni:ns) = do
  m <- getNodeStory pool ni
  nodeStoryIncs pool (Just m) ns

-- nodeStoryDec :: Pool PGS.Connection -> NodeListStory -> NodeId -> IO NodeListStory
-- nodeStoryDec pool ns@(NodeStory nls) ni = do
--   case Map.lookup ni nls of
--     Nothing -> do
--       _ <- nodeStoryRemove pool ni
--       pure ns
--     Just _ -> do
--       let ns' = Map.filterWithKey (\k _v -> k /= ni) nls
--       _ <- nodeStoryRemove pool ni
--       pure $ NodeStory ns'
------------------------------------

readNodeStoryEnv :: Pool PGS.Connection -> IO NodeStoryEnv
readNodeStoryEnv pool = do
  mvar <- nodeStoryVar pool Nothing []
  saver <- mkNodeStorySaver pool mvar
  -- let saver = modifyMVar_ mvar $ \mv' -> do
  --       writeNodeStories mv'
  --       return mv'
  pure $ NodeStoryEnv { _nse_var    = mvar
                      , _nse_saver  = saver
                      , _nse_getter = nodeStoryVar pool (Just mvar) }

nodeStoryVar :: Pool PGS.Connection -> Maybe (MVar NodeListStory) -> [NodeId] -> IO (MVar NodeListStory)
nodeStoryVar pool Nothing nIds = do
  state <- nodeStoryIncs pool Nothing nIds
  newMVar state
nodeStoryVar pool (Just mv) nIds = do
  _ <- modifyMVar_ mv $ \nsl -> (nodeStoryIncs pool (Just nsl) nIds)
  pure mv

-- TODO No debounce since this is IO stuff.
-- debounce is useful since it could delay the saving to some later
-- time, asynchronously and we keep operating on memory only.
mkNodeStorySaver :: Pool PGS.Connection -> MVar NodeListStory -> IO (IO ())
mkNodeStorySaver pool mvns = mkDebounce settings
  where
    settings = defaultDebounceSettings
                 { debounceAction = withMVar mvns (\ns -> writeNodeStories pool ns)
                 , debounceFreq = 1*minute
                 }
    minute = 60*second
    second = 10^(6 :: Int)
--mkNodeStorySaver pool mvns = withMVar mvns $ writeNodeStories pool

-- mkNodeStorySaver :: MVar NodeListStory -> Cmd err (Cmd err ())
-- mkNodeStorySaver mvns = mkDebounce settings
--   where
--     settings = defaultDebounceSettings
--                  { debounceAction = withMVar mvns (\ns -> writeNodeStories ns)
--                  , debounceFreq = 1 * minute
-- --                 , debounceEdge = trailingEdge -- Trigger on the trailing edge
--                  }
--     minute = 60 * second
--     second = 10^(6 :: Int)


-----------------------------------------
