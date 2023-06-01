{-|
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
Module      : Gargantext.Database.Query.Table.Node
Description : Main Tools of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Query.Table.Node
  where

import Control.Arrow (returnA)
import Control.Lens (set, view)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Opaleye hiding (FromField)
import Prelude hiding (null, id, map, sum)
import qualified PUBMED.Types as PUBMED

import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Core.Types.Query (Limit, Offset)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Hyperdata.Default
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Filter (limit', offset')
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Schema.Node
import Gargantext.Prelude hiding (sum, head)

import qualified Database.PostgreSQL.Simple as PGS


queryNodeSearchTable :: Select NodeSearchRead
queryNodeSearchTable = selectTable nodeTableSearch

selectNode :: Column SqlInt4 -> Select NodeRead
selectNode id' = proc () -> do
    row      <- queryNodeTable -< ()
    restrict -< _node_id row .== id'
    returnA  -< row

runGetNodes :: Select NodeRead -> Cmd err [Node HyperdataAny]
runGetNodes = runOpaQuery

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | order by publication date
-- Favorites (Bool), node_ngrams
selectNodesWith :: HasDBid NodeType
                => ParentId     -> Maybe NodeType
                -> Maybe Offset -> Maybe Limit   -> Select NodeRead
selectNodesWith parentId maybeNodeType maybeOffset maybeLimit =
        --offset' maybeOffset $ limit' maybeLimit $ orderBy (asc (hyperdataDocument_Publication_date . node_hyperdata)) $ selectNodesWith' parentId typeId
  limit' maybeLimit $ offset' maybeOffset
                    $ orderBy (asc _node_id)
                    $ selectNodesWith' parentId maybeNodeType

selectNodesWith' :: HasDBid NodeType
                 => ParentId -> Maybe NodeType -> Select NodeRead
selectNodesWith' parentId maybeNodeType = proc () -> do
    node' <- (proc () -> do
      row@(Node _ _ typeId _ parentId' _ _ _) <- queryNodeTable -< ()
      restrict -< parentId' .== (pgNodeId parentId)

      let typeId' = maybe 0 toDBid maybeNodeType

      restrict -< if typeId' > 0
                     then typeId   .== (sqlInt4 (typeId' :: Int))
                     else (sqlBool True)
      returnA  -< row ) -< ()
    returnA -< node'

deleteNode :: NodeId -> Cmd err Int
deleteNode n = mkCmd $ \conn ->
  fromIntegral <$> runDelete_ conn
                 (Delete nodeTable
                         (\(Node n_id _ _ _ _ _ _ _) -> n_id .== pgNodeId n)
                         rCount
                 )

deleteNodes :: [NodeId] -> Cmd err Int
deleteNodes ns = mkCmd $ \conn ->
  fromIntegral <$> runDelete_ conn
                   (Delete nodeTable
                           (\(Node n_id _ _ _ _ _ _ _) -> in_ ((map pgNodeId ns)) n_id)
                           rCount
                   )

-- TODO: NodeType should match with `a'
getNodesWith :: (JSONB a, HasDBid NodeType) => NodeId -> proxy a -> Maybe NodeType
             -> Maybe Offset -> Maybe Limit -> Cmd err [Node a]
getNodesWith parentId _ nodeType maybeOffset maybeLimit =
    runOpaQuery $ selectNodesWith parentId nodeType maybeOffset maybeLimit

-- TODO: Why is the second parameter ignored?
-- TODO: Why not use getNodesWith?
getNodesWithParentId :: (Hyperdata a, JSONB a)
                     => Maybe NodeId
                     -> Cmd err [Node a]
getNodesWithParentId n = runOpaQuery $ selectNodesWithParentID n'
  where
    n' = case n of
      Just n'' -> n''
      Nothing  -> 0


-- | Given a node id, find it's closest parent of given type
-- NOTE: This isn't too optimal: can make successive queries depending on how
-- deeply nested the child is.
getClosestParentIdByType :: HasDBid NodeType
                         => NodeId
                         -> NodeType
                         -> Cmd err (Maybe NodeId)
getClosestParentIdByType nId nType = do
  result <- runPGSQuery query (PGS.Only nId)
  case result of
    [(NodeId parentId, pTypename)] -> do
      if toDBid nType == pTypename then
        pure $ Just $ NodeId parentId
      else
        getClosestParentIdByType (NodeId parentId) nType
    _ -> pure Nothing
  where
    query :: PGS.Query
    query = [sql|
      SELECT n2.id, n2.typename
      FROM nodes n1
        JOIN nodes n2 ON n1.parent_id = n2.id
        WHERE n1.id = ?;
    |]

-- | Similar to `getClosestParentIdByType` but includes current node
-- in search too
getClosestParentIdByType' :: HasDBid NodeType
                          => NodeId
                          -> NodeType
                          -> Cmd err (Maybe NodeId)
getClosestParentIdByType' nId nType = do
  result <- runPGSQuery query (PGS.Only nId)
  case result of
    [(NodeId id, pTypename)] -> do
      if toDBid nType == pTypename then
        pure $ Just $ NodeId id
      else
        getClosestParentIdByType nId nType
    _ -> pure Nothing
  where
    query :: PGS.Query
    query = [sql|
      SELECT n.id, n.typename
      FROM nodes n
      WHERE n.id = ?;
    |]

-- | Given a node id, find all it's children (no matter how deep) of
-- given node type.
getChildrenByType :: HasDBid NodeType
                  => NodeId
                  -> NodeType
                  -> Cmd err [NodeId]
getChildrenByType nId nType = do
  result <- runPGSQuery query (PGS.Only nId)
  children_lst <- mapM (\(id, _) -> getChildrenByType id nType) result
  pure $ concat $ [fst <$> filter (\(_, pTypename) -> pTypename == toDBid nType) result] ++ children_lst
  where
    query :: PGS.Query
    query = [sql|
      SELECT n.id, n.typename
      FROM nodes n
      WHERE n.parent_id = ?;
    |]

------------------------------------------------------------------------
getDocumentsV3WithParentId :: HasDBid NodeType => NodeId -> Cmd err [Node HyperdataDocumentV3]
getDocumentsV3WithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeDocument)

-- TODO: merge with getDocumentsWithParentId by having a class IsHyperdataDocument
getDocumentsWithParentId :: HasDBid NodeType => NodeId -> Cmd err [Node HyperdataDocument]
getDocumentsWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeDocument)

getListsModelWithParentId :: HasDBid NodeType => NodeId -> Cmd err [Node HyperdataModel]
getListsModelWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeModel)

getCorporaWithParentId :: HasDBid NodeType => NodeId -> Cmd err [Node HyperdataCorpus]
getCorporaWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeCorpus)

------------------------------------------------------------------------
selectNodesWithParentID :: NodeId -> Select NodeRead
selectNodesWithParentID n = proc () -> do
    row@(Node _ _ _ _ parent_id _ _ _) <- queryNodeTable -< ()
    restrict -< parent_id .== (pgNodeId n)
    returnA -< row


------------------------------------------------------------------------
-- | Example of use:
-- runCmdReplEasy  (getNodesWithType NodeList (Proxy :: Proxy HyperdataList))
getNodesWithType :: (HasNodeError err, JSONB a, HasDBid NodeType) => NodeType -> proxy a -> Cmd err [Node a]
getNodesWithType nt _ = runOpaQuery $ selectNodesWithType nt
  where
    selectNodesWithType ::  HasDBid NodeType
                         => NodeType -> Select NodeRead
    selectNodesWithType nt' = proc () -> do
        row@(Node _ _ tn _ _ _ _ _) <- queryNodeTable -< ()
        restrict -< tn .== (sqlInt4 $ toDBid nt')
        returnA -< row

getNodesIdWithType :: (HasNodeError err, HasDBid NodeType) => NodeType -> Cmd err [NodeId]
getNodesIdWithType nt = do
  ns <- runOpaQuery $ selectNodesIdWithType nt
  pure (map NodeId ns)

selectNodesIdWithType :: HasDBid NodeType
                      => NodeType -> Select (Column SqlInt4)
selectNodesIdWithType nt = proc () -> do
    row@(Node _ _ tn _ _ _ _ _) <- queryNodeTable -< ()
    restrict -< tn .== (sqlInt4 $ toDBid nt)
    returnA -< _node_id row

------------------------------------------------------------------------

nodeExists :: (HasNodeError err) => NodeId -> Cmd err Bool
nodeExists nId = (== [PGS.Only True])
  <$> runPGSQuery [sql|SELECT true FROM nodes WHERE id = ? |] (PGS.Only nId)

getNode :: HasNodeError err => NodeId -> Cmd err (Node Value)
getNode nId = do
  maybeNode <- headMay <$> runOpaQuery (selectNode (pgNodeId nId))
  case maybeNode of
    Nothing -> nodeError (DoesNotExist nId)
    Just  r -> pure r

getNodeWith :: (HasNodeError err, JSONB a)
            => NodeId -> proxy a -> Cmd err (Node a)
getNodeWith nId _ = do
  maybeNode <- headMay <$> runOpaQuery (selectNode (pgNodeId nId))
  case maybeNode of
    Nothing -> nodeError (DoesNotExist nId)
    Just  r -> pure r


------------------------------------------------------------------------
-- | Sugar to insert Node with NodeType in Database
insertDefaultNode :: HasDBid NodeType
                  => NodeType -> ParentId -> UserId -> Cmd err [NodeId]
insertDefaultNode nt p u = insertNode nt Nothing Nothing p u

insertDefaultNodeIfNotExists :: HasDBid NodeType
                             => NodeType -> ParentId -> UserId -> Cmd err [NodeId]
insertDefaultNodeIfNotExists nt p u = do
  children <- getChildrenByType p nt
  case children of
    [] -> insertDefaultNode nt p u
    xs -> pure xs

insertNode :: HasDBid NodeType
           => NodeType -> Maybe Name -> Maybe DefaultHyperdata -> ParentId -> UserId -> Cmd err [NodeId]
insertNode nt n h p u = insertNodesR [nodeW nt n h p u]

nodeW ::  HasDBid NodeType
       => NodeType -> Maybe Name -> Maybe DefaultHyperdata -> ParentId -> UserId -> NodeWrite
nodeW nt n h p u = node nt n' h' (Just p) u
  where
    n' = fromMaybe (defaultName nt) n
    h' = maybe     (defaultHyperdata nt) identity h

------------------------------------------------------------------------
node :: (ToJSON a, Hyperdata a, HasDBid NodeType)
     => NodeType
     -> Name
     -> a
     -> Maybe ParentId
     -> UserId
     -> NodeWrite
node nodeType name hyperData parentId userId =
  Node Nothing Nothing
       (sqlInt4 typeId)
       (sqlInt4 userId)
       (pgNodeId <$> parentId)
       (sqlStrictText name)
       Nothing
       (sqlJSONB $ cs $ encode hyperData)
    where
      typeId = toDBid nodeType

                  -------------------------------
insertNodes :: [NodeWrite] -> Cmd err Int64
insertNodes ns = mkCmd $ \conn -> runInsert_ conn $ Insert nodeTable ns rCount Nothing

{-
insertNodes' :: [Node a] -> Cmd err Int64
insertNodes' ns = mkCmd $ \conn -> runInsert_ conn
                        $ Insert nodeTable ns' rCount Nothing
  where
    ns' :: [NodeWrite]
    ns' = map (\(Node i t u p n d h)
                -> Node (pgNodeId          <$> i)
                        (sqlInt4 $ toDBid      t)
                        (sqlInt4                u)
                        (pgNodeId          <$> p)
                        (sqlStrictText          n)
                        (pgUTCTime         <$> d)
                        (pgJSONB $ cs $ encode h)
              ) ns
-}

insertNodesR :: [NodeWrite] -> Cmd err [NodeId]
insertNodesR ns = mkCmd $ \conn ->
  runInsert_ conn (Insert nodeTable ns (rReturning (\(Node i _ _ _ _ _ _ _) -> i)) Nothing)

insertNodesWithParent :: Maybe ParentId -> [NodeWrite] -> Cmd err Int64
insertNodesWithParent pid ns = insertNodes (set node_parent_id (pgNodeId <$> pid) <$> ns)

insertNodesWithParentR :: Maybe ParentId -> [NodeWrite] -> Cmd err [NodeId]
insertNodesWithParentR pid ns = insertNodesR (set node_parent_id (pgNodeId <$> pid) <$> ns)

getCorpusPubmedAPIKey :: NodeId -> Cmd err (Maybe PUBMED.APIKey)
getCorpusPubmedAPIKey cId = do
  res <- runPGSQuery query params
  pure $ (\(PGS.Only apiKey) -> apiKey) <$> head res
  where
    query :: PGS.Query
    query = [sql|
                SELECT hyperdata -> 'pubmed_api_key'
                FROM nodes
                WHERE id = ?
            |]
    params = PGS.Only cId

updateCorpusPubmedAPIKey :: NodeId -> Maybe PUBMED.APIKey -> Cmd err Int64
updateCorpusPubmedAPIKey cId mAPIKey =
  execPGSQuery query params
  where
    query :: PGS.Query
    query = [sql|
                UPDATE nodes
                SET hyperdata = hyperdata || ?
                WHERE id = ?
            |]
    params = (encode $ object [ "pubmed_api_key" .= mAPIKey ], cId)
------------------------------------------------------------------------
-- TODO
-- currently this function removes the child relation
-- needs a Temporary type between Node' and NodeWriteT

node2table :: HasDBid NodeType
           => UserId -> Maybe ParentId -> Node' -> NodeWrite
node2table uid pid (Node' nt txt v []) = Node Nothing Nothing (sqlInt4 $ toDBid nt) (sqlInt4 uid) (fmap pgNodeId pid) (sqlStrictText txt) Nothing (sqlStrictJSONB $ cs $ encode v)
node2table _ _ (Node' _ _ _ _) = panic "node2table: should not happen, Tree insert not implemented yet"


data Node' = Node' { _n_type :: NodeType
                   , _n_name :: Text
                   , _n_data :: Value
                   , _n_children :: [Node']
                   } deriving (Show)

mkNodes :: [NodeWrite] -> Cmd err Int64
mkNodes ns = mkCmd $ \conn -> runInsert_ conn
                   $ Insert nodeTable ns rCount Nothing

mkNodeR :: [NodeWrite] -> Cmd err [NodeId]
mkNodeR ns = mkCmd $ \conn -> runInsert_ conn $ Insert nodeTable ns (rReturning _node_id) Nothing

------------------------------------------------------------------------
childWith ::  HasDBid NodeType
           => UserId -> ParentId -> Node' -> NodeWrite
childWith uId pId (Node' NodeDocument txt v []) = node2table uId (Just pId) (Node' NodeDocument txt v [])
childWith uId pId (Node' NodeContact  txt v []) = node2table uId (Just pId) (Node' NodeContact txt v [])
childWith _   _   (Node' _        _   _ _) = panic "This NodeType can not be a child"


-- =================================================================== --
-- |
-- CorpusDocument is a corpus made from a set of documents
-- CorpusContact  is a corpus made from a set of contacts (syn of Annuaire)
data CorpusType = CorpusDocument | CorpusContact

class MkCorpus a
  where
    mk :: HasDBid NodeType => Maybe Name -> Maybe a -> ParentId -> UserId -> Cmd err [NodeId]

instance MkCorpus HyperdataCorpus
  where
    mk n Nothing  p u = insertNode NodeCorpus n Nothing p u
    mk n (Just h) p u = insertNode NodeCorpus n (Just $ DefaultCorpus h) p u


instance MkCorpus HyperdataAnnuaire
  where
    mk n Nothing  p u = insertNode NodeCorpus   n Nothing p u
    mk n (Just h) p u = insertNode NodeAnnuaire n (Just $ DefaultAnnuaire h) p u


getOrMkList :: (HasNodeError err, HasDBid NodeType)
            => ParentId
            -> UserId
            -> Cmd err ListId
getOrMkList pId uId =
  maybe (mkList' pId uId) (pure . view node_id) . headMay =<< getListsWithParentId pId
    where
      mkList' pId' uId' = maybe (nodeError MkNode) pure . headMay =<< insertDefaultNode NodeList pId' uId'

-- | TODO remove defaultList
defaultList :: (HasNodeError err, HasDBid NodeType) => CorpusId -> Cmd err ListId
defaultList cId =
  maybe (nodeError (NoListFound cId)) (pure . view node_id) . headMay =<< getListsWithParentId cId

defaultListMaybe :: (HasNodeError err, HasDBid NodeType) => CorpusId -> Cmd err (Maybe NodeId)
defaultListMaybe cId = headMay <$> map (view node_id ) <$> getListsWithParentId cId

getListsWithParentId :: HasDBid NodeType => NodeId -> Cmd err [Node HyperdataList]
getListsWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeList)
