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

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Query.Table.Node
  where

import Control.Arrow (returnA)
import Control.Lens (set, view)
import Data.Aeson
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Text (Text)
import GHC.Int (Int64)
import Gargantext.Core.Types
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Hyperdata.Default
import Gargantext.Database.Admin.Types.Node (NodeType(..), defaultName)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Filter (limit', offset')
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Schema.Node
import Gargantext.Prelude hiding (sum, head)
import Opaleye hiding (FromField)
import Opaleye.Internal.QueryArr (Query)
import Prelude hiding (null, id, map, sum)


queryNodeSearchTable :: Query NodeSearchRead
queryNodeSearchTable = queryTable nodeTableSearch

selectNode :: Column PGInt4 -> Query NodeRead
selectNode id = proc () -> do
    row      <- queryNodeTable -< ()
    restrict -< _node_id row .== id
    returnA  -< row

runGetNodes :: Query NodeRead -> Cmd err [Node HyperdataAny]
runGetNodes = runOpaQuery

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | order by publication date
-- Favorites (Bool), node_ngrams
selectNodesWith :: ParentId     -> Maybe NodeType
                -> Maybe Offset -> Maybe Limit   -> Query NodeRead
selectNodesWith parentId maybeNodeType maybeOffset maybeLimit =
        --offset' maybeOffset $ limit' maybeLimit $ orderBy (asc (hyperdataDocument_Publication_date . node_hyperdata)) $ selectNodesWith' parentId typeId
  limit' maybeLimit $ offset' maybeOffset
                    $ orderBy (asc _node_id)
                    $ selectNodesWith' parentId maybeNodeType

selectNodesWith' :: ParentId -> Maybe NodeType -> Query NodeRead
selectNodesWith' parentId maybeNodeType = proc () -> do
    node <- (proc () -> do
      row@(Node _ typeId _ parentId' _ _ _) <- queryNodeTable -< ()
      restrict -< parentId' .== (pgNodeId parentId)

      let typeId' = maybe 0 nodeTypeId maybeNodeType

      restrict -< if typeId' > 0
                     then typeId   .== (pgInt4 (typeId' :: Int))
                     else (pgBool True)
      returnA  -< row ) -< ()
    returnA -< node

deleteNode :: NodeId -> Cmd err Int
deleteNode n = mkCmd $ \conn ->
  fromIntegral <$> runDelete conn nodeTable
                 (\(Node n_id _ _ _ _ _ _) -> n_id .== pgNodeId n)

deleteNodes :: [NodeId] -> Cmd err Int
deleteNodes ns = mkCmd $ \conn ->
  fromIntegral <$> runDelete conn nodeTable
                   (\(Node n_id _ _ _ _ _ _) -> in_ ((map pgNodeId ns)) n_id)

-- TODO: NodeType should match with `a'
getNodesWith :: JSONB a => NodeId -> proxy a -> Maybe NodeType
             -> Maybe Offset -> Maybe Limit -> Cmd err [Node a]
getNodesWith parentId _ nodeType maybeOffset maybeLimit =
    runOpaQuery $ selectNodesWith parentId nodeType maybeOffset maybeLimit

-- TODO: Why is the second parameter ignored?
-- TODO: Why not use getNodesWith?
getNodesWithParentId :: (Hyperdata a, QueryRunnerColumnDefault PGJsonb a)
                     => Maybe NodeId
                     -> Cmd err [Node a]
getNodesWithParentId n = runOpaQuery $ selectNodesWithParentID n'
  where
    n' = case n of
      Just n'' -> n''
      Nothing  -> 0

------------------------------------------------------------------------
getDocumentsV3WithParentId :: NodeId -> Cmd err [Node HyperdataDocumentV3]
getDocumentsV3WithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeDocument)

-- TODO: merge with getDocumentsWithParentId by having a class IsHyperdataDocument
getDocumentsWithParentId :: NodeId -> Cmd err [Node HyperdataDocument]
getDocumentsWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeDocument)

getListsModelWithParentId :: NodeId -> Cmd err [Node HyperdataModel]
getListsModelWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeModel)

getCorporaWithParentId :: NodeId -> Cmd err [Node HyperdataCorpus]
getCorporaWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeCorpus)

------------------------------------------------------------------------
selectNodesWithParentID :: NodeId -> Query NodeRead
selectNodesWithParentID n = proc () -> do
    row@(Node _ _ _ parent_id _ _ _) <- queryNodeTable -< ()
    restrict -< parent_id .== (pgNodeId n)
    returnA -< row

selectNodesWithType :: Column PGInt4 -> Query NodeRead
selectNodesWithType type_id = proc () -> do
    row@(Node _ tn _ _ _ _ _) <- queryNodeTable -< ()
    restrict -< tn .== type_id
    returnA -< row

type JSONB = QueryRunnerColumnDefault PGJsonb


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
insertDefaultNode :: NodeType -> ParentId -> UserId -> Cmd err [NodeId]
insertDefaultNode nt p u = insertNode nt Nothing Nothing p u

insertNode :: NodeType -> Maybe Name -> Maybe DefaultHyperdata -> ParentId -> UserId -> Cmd err [NodeId]
insertNode nt n h p u = insertNodesR [nodeW nt n h p u]

nodeW :: NodeType -> Maybe Name -> Maybe DefaultHyperdata -> ParentId -> UserId -> NodeWrite
nodeW nt n h p u = node nt n' h' (Just p) u
  where
    n' = fromMaybe (defaultName nt) n
    h' = maybe     (defaultHyperdata nt) identity h

------------------------------------------------------------------------
node :: (ToJSON a, Hyperdata a)
     => NodeType
     -> Name
     -> a
     -> Maybe ParentId
     -> UserId
     -> NodeWrite
node nodeType name hyperData parentId userId =
  Node Nothing
       (pgInt4 typeId)
       (pgInt4 userId)
       (pgNodeId <$> parentId)
       (pgStrictText name)
       Nothing
       (pgJSONB $ cs $ encode hyperData)
    where
      typeId = nodeTypeId nodeType

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
                        (pgInt4 $ nodeTypeId   t)
                        (pgInt4                u)
                        (pgNodeId          <$> p)
                        (pgStrictText          n)
                        (pgUTCTime         <$> d)
                        (pgJSONB $ cs $ encode h)
              ) ns
-}

insertNodesR :: [NodeWrite] -> Cmd err [NodeId]
insertNodesR ns = mkCmd $ \conn ->
  runInsert_ conn (Insert nodeTable ns (rReturning (\(Node i _ _ _ _ _ _) -> i)) Nothing)

insertNodesWithParent :: Maybe ParentId -> [NodeWrite] -> Cmd err Int64
insertNodesWithParent pid ns = insertNodes (set node_parentId (pgNodeId <$> pid) <$> ns)

insertNodesWithParentR :: Maybe ParentId -> [NodeWrite] -> Cmd err [NodeId]
insertNodesWithParentR pid ns = insertNodesR (set node_parentId (pgNodeId <$> pid) <$> ns)
------------------------------------------------------------------------
-- TODO
-- currently this function removes the child relation
-- needs a Temporary type between Node' and NodeWriteT

node2table :: UserId -> Maybe ParentId -> Node' -> NodeWrite
node2table uid pid (Node' nt txt v []) = Node Nothing (pgInt4 $ nodeTypeId nt) (pgInt4 uid) (fmap pgNodeId pid) (pgStrictText txt) Nothing (pgStrictJSONB $ cs $ encode v)
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
childWith :: UserId -> ParentId -> Node' -> NodeWrite
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
    mk :: Maybe Name -> Maybe a -> ParentId -> UserId -> Cmd err [NodeId]

instance MkCorpus HyperdataCorpus
  where
    mk n Nothing  p u = insertNode NodeCorpus n Nothing p u
    mk n (Just h) p u = insertNode NodeCorpus n (Just $ DefaultCorpus h) p u


instance MkCorpus HyperdataAnnuaire
  where
    mk n Nothing  p u = insertNode NodeCorpus   n Nothing p u
    mk n (Just h) p u = insertNode NodeAnnuaire n (Just $ DefaultAnnuaire h) p u


getOrMkList :: HasNodeError err
            => ParentId
            -> UserId
            -> Cmd err ListId
getOrMkList pId uId =
  maybe (mkList' pId uId) (pure . view node_id) . headMay =<< getListsWithParentId pId
    where
      mkList' pId uId = maybe (nodeError MkNode) pure . headMay =<< insertDefaultNode NodeList pId uId

-- | TODO remove defaultList
defaultList :: HasNodeError err => CorpusId -> Cmd err ListId
defaultList cId =
  maybe (nodeError NoListFound) (pure . view node_id) . headMay =<< getListsWithParentId cId


getListsWithParentId :: NodeId -> Cmd err [Node HyperdataList]
getListsWithParentId n = runOpaQuery $ selectNodesWith' n (Just NodeList)

