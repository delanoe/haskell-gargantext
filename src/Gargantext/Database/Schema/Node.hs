{-|
Module      : Gargantext.Database.Schema.Node
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.Node where

import Control.Arrow (returnA)
import Control.Lens (set)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Int (Int64)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Core.Types.Main (UserId)
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Queries.Filter (limit', offset')
import Gargantext.Database.Types.Node (NodeType, defaultCorpus, Hyperdata)
import Gargantext.Database.Utils
import Gargantext.Prelude hiding (sum, head)
import Opaleye hiding (FromField)
import Opaleye.Internal.QueryArr (Query)
import Prelude hiding (null, id, map, sum)
import qualified Data.ByteString      as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Profunctor.Product as PP

------------------------------------------------------------------------
instance FromField HyperdataAny
  where
    fromField = fromField'

instance FromField HyperdataCorpus
  where
    fromField = fromField'

instance FromField HyperdataDocument
  where
    fromField = fromField'

instance FromField HyperdataDocumentV3
  where
    fromField = fromField'

instance FromField HyperdataUser
  where
    fromField = fromField'

instance FromField HyperdataList
  where
    fromField = fromField'

instance FromField HyperdataGraph
  where
    fromField = fromField'

instance FromField HyperdataAnnuaire
  where
    fromField = fromField'
------------------------------------------------------------------------
instance QueryRunnerColumnDefault PGJsonb HyperdataAny
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocument
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocumentV3
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataUser
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataList
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataGraph
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataAnnuaire
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGTSVector (Maybe TSVector)
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


------------------------------------------------------------------------

-- WIP
-- TODO Classe HasDefault where
-- default NodeType = Hyperdata
------------------------------------------------------------------------
$(makeAdaptorAndInstance "pNode" ''NodePoly)
$(makeLensesWith abbreviatedFields ''NodePoly)
$(makeAdaptorAndInstance "pNodeSearch" ''NodePolySearch)
$(makeLensesWith abbreviatedFields ''NodePolySearch)

type NodeWrite = NodePoly  (Maybe (Column  PGInt4              ))
                                  (Column  PGInt4               )
                                  (Column  PGInt4               )
                                  (Column (Nullable PGInt4     ))
                                  (Column (PGText              ))
                                  (Maybe  (Column PGTimestamptz))
                                  (Column  PGJsonb              )

type NodeRead = NodePoly  (Column  PGInt4           )
                          (Column  PGInt4           )
                          (Column  PGInt4           )
                          (Column (Nullable PGInt4 ))
                          (Column (PGText          ))
                          (Column PGTimestamptz     )
                          (Column PGJsonb) 


type NodeReadNull = NodePoly  (Column  (Nullable PGInt4           ))
                              (Column  (Nullable PGInt4           ))
                              (Column  (Nullable PGInt4           ))
                              (Column (Nullable PGInt4 ))
                              (Column (Nullable PGText          ))
                              (Column (Nullable PGTimestamptz     ))
                              (Column (Nullable PGJsonb))

nodeTable :: Table NodeWrite NodeRead
nodeTable = Table "nodes" (pNode Node { _node_id         = optional "id"
                                      , _node_typename   = required "typename"
                                      , _node_userId     = required "user_id"
                                      
                                      , _node_parentId   = required "parent_id"
                                      , _node_name       = required "name"
                                      , _node_date       = optional "date"
                                      
                                      , _node_hyperdata  = required "hyperdata"
                                      }
                            )

queryNodeTable :: Query NodeRead
queryNodeTable = queryTable nodeTable

-- | TODO remove below
type NodeWrite' = NodePoly (Maybe Int) Int Int (Maybe ParentId) Text (Maybe UTCTime) ByteString

--{-
nodeTable' :: Table (Maybe (Column PGInt4)
                    ,       Column PGInt4
                    ,       Column PGInt4
                    ,Maybe (Column PGInt4)
                    ,       Column PGText
                    ,Maybe (Column PGTimestamptz)
                    ,       Column PGJsonb
                    )
                    ((Column PGInt4)
                    ,       Column PGInt4
                    ,       Column PGInt4
                    ,       Column PGInt4
                    ,       Column PGText
                    ,(Column PGTimestamptz)
                    ,       Column PGJsonb
                    )

nodeTable' = Table "nodes" (PP.p7 ( optional "id"
                               , required "typename"
                               , required "user_id"
                               
                               , optional "parent_id"
                               , required "name"
                               , optional "date"
                               
                               , required "hyperdata"
                               )
                            )
--}
------------------------------------------------------------------------
-- | Node(Read|Write)Search is slower than Node(Write|Read) use it
-- for full text search only

type NodeSearchWrite = NodePolySearch  (Maybe (Column  PGInt4              ))
                                  (Column  PGInt4               )
                                  (Column  PGInt4               )
                                  (Column (Nullable PGInt4     ))
                                  (Column (PGText              ))
                                  (Maybe  (Column PGTimestamptz))
                                  (Column  PGJsonb              )
                                  (Maybe (Column PGTSVector))

type NodeSearchRead = NodePolySearch  (Column  PGInt4           )
                          (Column  PGInt4           )
                          (Column  PGInt4           )
                          (Column (Nullable PGInt4 ))
                          (Column (PGText          ))
                          (Column PGTimestamptz     )
                          (Column PGJsonb) 
                          (Column PGTSVector)


type NodeSearchReadNull = NodePolySearch  (Column  (Nullable PGInt4           ))
                              (Column  (Nullable PGInt4           ))
                              (Column  (Nullable PGInt4           ))
                              (Column (Nullable PGInt4 ))
                              (Column (Nullable PGText          ))
                              (Column (Nullable PGTimestamptz     ))
                              (Column (Nullable PGJsonb))
                              (Column (Nullable PGTSVector))


--{-
nodeTableSearch :: Table NodeSearchWrite NodeSearchRead
nodeTableSearch = Table "nodes" (pNodeSearch NodeSearch { _ns_id         = optional "id"
                                      , _ns_typename   = required "typename"
                                      , _ns_userId     = required "user_id"
                                      
                                      , _ns_parentId   = required "parent_id"
                                      , _ns_name       = required "name"
                                      , _ns_date       = optional "date"
                                      
                                      , _ns_hyperdata  = required "hyperdata"
                                      , _ns_search     = optional "search"
                                      }
                            )
--}

queryNodeSearchTable :: Query NodeSearchRead
queryNodeSearchTable = queryTable nodeTableSearch


selectNode :: Column PGInt4 -> Query NodeRead
selectNode id = proc () -> do
    row <- queryNodeTable -< ()
    restrict -< _node_id row .== id
    returnA -< row


runGetNodes :: Query NodeRead -> Cmd [NodeAny]
runGetNodes q = mkCmd $ \conn -> runQuery conn q

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
      restrict -< parentId' .== (toNullable $ pgInt4 parentId)

      let typeId' = maybe 0 nodeTypeId maybeNodeType

      restrict -< if typeId' > 0
                     then typeId   .== (pgInt4 (typeId' :: Int))
                     else (pgBool True)
      returnA  -< row ) -< ()
    returnA -< node



-- deleteNode :: (MonadReader Connection m, MonadIO m) => Int -> m Int
-- deleteNode :: Int -> Cmd' Int

deleteNode :: Int -> Cmd Int
deleteNode n = mkCmd $ \conn ->
  fromIntegral <$> runDelete conn nodeTable
                 (\(Node n_id _ _ _ _ _ _) -> n_id .== pgInt4 n)

deleteNodes :: [Int] -> Cmd Int
deleteNodes ns = mkCmd $ \conn ->
  fromIntegral <$> runDelete conn nodeTable
                   (\(Node n_id _ _ _ _ _ _) -> in_ ((map pgInt4 ns)) n_id)


getNodesWith :: JSONB a => Connection -> Int -> proxy a -> Maybe NodeType
             -> Maybe Offset -> Maybe Limit -> IO [Node a]
getNodesWith conn parentId _ nodeType maybeOffset maybeLimit =
    runQuery conn $ selectNodesWith parentId nodeType maybeOffset maybeLimit


-- NP check type
getNodesWithParentId :: Int
                     -> Maybe Text -> Connection -> IO [NodeAny]
getNodesWithParentId n _ conn = runQuery conn $ selectNodesWithParentID n

getNodesWithParentId' :: Int
                     -> Maybe Text -> Connection -> IO [NodeAny]
getNodesWithParentId' n _ conn = runQuery conn $ selectNodesWithParentID n


------------------------------------------------------------------------
getDocumentsV3WithParentId :: Connection -> Int -> IO [Node HyperdataDocumentV3]
getDocumentsV3WithParentId conn n = runQuery conn $ selectNodesWith' n (Just NodeDocument)

getDocumentsWithParentId :: Connection -> Int -> IO [Node HyperdataDocument]
getDocumentsWithParentId conn n = runQuery conn $ selectNodesWith' n (Just NodeDocument)

getListsWithParentId :: Connection -> Int -> IO [Node HyperdataList]
getListsWithParentId conn n = runQuery conn $ selectNodesWith' n (Just NodeList)

getCorporaWithParentId :: Connection -> Int -> IO [Node HyperdataCorpus]
getCorporaWithParentId conn n = runQuery conn $ selectNodesWith' n (Just NodeCorpus)

getCorporaWithParentId' :: Int -> Cmd [Node HyperdataCorpus]
getCorporaWithParentId' n = mkCmd $ \conn -> runQuery conn $ selectNodesWith' n (Just NodeCorpus)


------------------------------------------------------------------------
selectNodesWithParentID :: Int -> Query NodeRead
selectNodesWithParentID n = proc () -> do
    row@(Node _ _ _ parent_id _ _ _) <- queryNodeTable -< ()
    restrict -< if n > 0
      then parent_id .== (toNullable $ pgInt4 n)
      else isNull parent_id
    returnA -< row

selectNodesWithType :: Column PGInt4 -> Query NodeRead
selectNodesWithType type_id = proc () -> do
    row@(Node _ tn _ _ _ _ _) <- queryNodeTable -< ()
    restrict -< tn .== type_id
    returnA -< row

type JSONB = QueryRunnerColumnDefault PGJsonb

getNode :: JSONB a => Connection -> Int -> proxy a -> IO (Node a)
getNode conn id _ = do
    fromMaybe (error $ "Node does node exist: " <> show id) . headMay <$> runQuery conn (limit 1 $ selectNode (pgInt4 id))

getNodesWithType :: Connection -> Column PGInt4 -> IO [Node HyperdataDocument]
getNodesWithType conn type_id = do
    runQuery conn $ selectNodesWithType type_id

------------------------------------------------------------------------

------------------------------------------------------------------------
defaultUser :: HyperdataUser
defaultUser = HyperdataUser (Just $ (pack . show) EN)

nodeUserW :: Maybe Name -> Maybe HyperdataUser -> UserId -> NodeWrite'
nodeUserW maybeName maybeHyperdata = node NodeUser name user Nothing
  where
    name = maybe "User" identity maybeName
    user = maybe defaultUser identity maybeHyperdata
------------------------------------------------------------------------
defaultFolder :: HyperdataFolder
defaultFolder = HyperdataFolder (Just "Markdown Description")

nodeFolderW :: Maybe Name -> Maybe HyperdataFolder -> ParentId -> UserId -> NodeWrite'
nodeFolderW maybeName maybeFolder pid = node NodeFolder name folder (Just pid)
  where
    name   = maybe "Folder" identity maybeName
    folder = maybe defaultFolder identity maybeFolder
------------------------------------------------------------------------
nodeCorpusW :: Maybe Name -> Maybe HyperdataCorpus -> ParentId -> UserId -> NodeWrite'
nodeCorpusW maybeName maybeCorpus pId = node NodeCorpus name corpus (Just pId)
  where
    name   = maybe "Corpus" identity maybeName
    corpus = maybe defaultCorpus identity maybeCorpus
                   --------------------------
defaultDocument :: HyperdataDocument
defaultDocument = hyperdataDocument

nodeDocumentW :: Maybe Name -> Maybe HyperdataDocument -> CorpusId -> UserId -> NodeWrite'
nodeDocumentW maybeName maybeDocument cId = node NodeDocument name doc (Just cId)
  where
    name = maybe "Document" identity maybeName
    doc  = maybe defaultDocument identity maybeDocument
------------------------------------------------------------------------
defaultAnnuaire :: HyperdataAnnuaire
defaultAnnuaire = HyperdataAnnuaire (Just "Title") (Just "Description")

nodeAnnuaireW :: Maybe Name -> Maybe HyperdataAnnuaire -> ParentId -> UserId -> NodeWrite'
nodeAnnuaireW maybeName maybeAnnuaire pId = node NodeAnnuaire name annuaire (Just pId)
  where
    name     = maybe "Annuaire" identity maybeName
    annuaire = maybe defaultAnnuaire identity maybeAnnuaire
                   --------------------------

------------------------------------------------------------------------
arbitraryList :: HyperdataList
arbitraryList = HyperdataList (Just "Preferences")

nodeListW :: Maybe Name -> Maybe HyperdataList -> ParentId -> UserId -> NodeWrite'
nodeListW maybeName maybeList pId = node NodeList name list (Just pId)
  where
    name = maybe "Listes" identity maybeName
    list = maybe arbitraryList identity maybeList

------------------------------------------------------------------------
arbitraryGraph :: HyperdataGraph
arbitraryGraph = HyperdataGraph (Just "Preferences")

nodeGraphW :: Maybe Name -> Maybe HyperdataGraph -> ParentId -> UserId -> NodeWrite'
nodeGraphW maybeName maybeGraph pId = node NodeGraph name graph (Just pId)
  where
    name = maybe "Graph" identity maybeName
    graph = maybe arbitraryGraph identity maybeGraph

------------------------------------------------------------------------

arbitraryDashboard :: HyperdataDashboard
arbitraryDashboard = HyperdataDashboard (Just "Preferences")

nodeDashboardW :: Maybe Name -> Maybe HyperdataDashboard -> ParentId -> UserId -> NodeWrite'
nodeDashboardW maybeName maybeDashboard pId = node NodeDashboard name dashboard (Just pId)
  where
    name = maybe "Dashboard" identity maybeName
    dashboard = maybe arbitraryDashboard identity maybeDashboard



------------------------------------------------------------------------
node :: (ToJSON a, Hyperdata a) => NodeType -> Name -> a -> Maybe ParentId -> UserId -> NodeWrite'
node nodeType name hyperData parentId userId = Node Nothing typeId userId parentId name Nothing byteData
  where
    typeId = nodeTypeId nodeType
    byteData = DB.pack . DBL.unpack $ encode hyperData

                  -------------------------------
node2row :: (Functor maybe1, Functor maybe2, Functor maybe3) =>
              NodePoly (maybe1 Int) Int  Int
                       (maybe2 Int) Text (maybe3 UTCTime)
                       ByteString
                    -> ( maybe1 (Column PGInt4), Column PGInt4, Column PGInt4
                       , maybe2 (Column PGInt4), Column PGText, maybe3 (Column PGTimestamptz)
                     , Column PGJsonb)
node2row (Node id tn ud pid nm dt hp) = ((pgInt4       <$> id)
                                           ,(pgInt4           tn)
                                           ,(pgInt4           ud)
                                           
                                           ,(pgInt4       <$> pid)
                                           ,(pgStrictText     nm)
                                           ,(pgUTCTime    <$> dt)
                                           
                                           ,(pgStrictJSONB    hp)
                                           )
------------------------------------------------------------------------
insertNodesR' :: [NodeWrite'] -> Cmd [Int]
insertNodesR' ns = mkCmd $ \c -> insertNodesR ns c

insertNodes :: [NodeWrite'] -> Connection -> IO Int64
insertNodes ns conn = runInsertMany conn nodeTable' (map node2row ns)

insertNodesR :: [NodeWrite'] -> Connection -> IO [Int]
insertNodesR ns conn = runInsertManyReturning conn nodeTable' (map node2row ns) (\(i,_,_,_,_,_,_) -> i)
                       -------------------------
insertNodesWithParent :: Maybe ParentId -> [NodeWrite'] -> Connection -> IO Int64
insertNodesWithParent pid ns conn = insertNodes (map (set node_parentId pid) ns) conn

insertNodesWithParentR :: Maybe ParentId -> [NodeWrite'] -> Connection -> IO [Int]
insertNodesWithParentR pid ns conn = insertNodesR (map (set node_parentId pid) ns) conn
------------------------------------------------------------------------
-- TODO Hierachy of Nodes
-- post and get same types Node' and update if changes

{- TODO semantic to achieve
post c uid pid [ Node' NodeCorpus "name" "{}" []
               , Node' NodeFolder "name" "{}" [Node' NodeCorpus "test 2" "" [ Node' NodeDocument "title" "metaData" []
                                                                    , Node' NodeDocument "title" "jsonData" []
                                                                    ]
                                          ]
               ]
-}
------------------------------------------------------------------------

-- TODO
-- currently this function remove the child relation
-- needs a Temporary type between Node' and NodeWriteT
node2table :: UserId -> Maybe ParentId -> Node' -> NodeWriteT
node2table uid pid (Node' nt txt v []) = ( Nothing, (pgInt4$ nodeTypeId nt), (pgInt4 uid), (fmap pgInt4 pid)
                                         , pgStrictText txt, Nothing, pgStrictJSONB $ DB.pack $ DBL.unpack $ encode v)
node2table _ _ (Node' _ _ _ _) = panic "node2table: should not happen, Tree insert not implemented yet"


data Node' = Node' { _n_type :: NodeType
                   , _n_name :: Text
                   , _n_data :: Value
                   , _n_children :: [Node']
                   } deriving (Show)

-- | TODO NodeWriteT -> NodeWrite
type NodeWriteT =  ( Maybe (Column PGInt4)
                   ,        Column PGInt4
                   ,        Column PGInt4
                   , Maybe (Column PGInt4)
                   ,        Column PGText
                   , Maybe (Column PGTimestamptz)
                   ,        Column PGJsonb
                   )

mkNode' :: [NodeWrite] -> Cmd Int64
mkNode' ns = mkCmd $ \conn -> runInsertMany conn nodeTable ns

-- TODO: replace mkNodeR'
mkNodeR'' :: [NodeWrite] -> Cmd [Int]
mkNodeR'' ns = mkCmd $ \conn -> runInsertManyReturning conn nodeTable ns (_node_id)

mkNodeR' :: [NodeWriteT] -> Cmd [Int]
mkNodeR' ns = mkCmd $ \conn -> runInsertManyReturning conn nodeTable' ns (\(i,_,_,_,_,_,_) -> i)

------------------------------------------------------------------------

data NewNode = NewNode { _newNodeId :: Int
                       , _newNodeChildren :: [Int] }

-- | postNode
postNode :: UserId -> Maybe ParentId -> Node' -> Cmd NewNode
postNode uid pid (Node' nt txt v []) = do
  pids <- mkNodeR' [node2table uid pid (Node' nt txt v [])]
  case pids of
    [pid] -> pure $ NewNode pid []
    _ -> panic "postNode: only one pid expected"

postNode uid pid (Node' NodeCorpus txt v ns) = do
  NewNode pid' _ <- postNode uid pid (Node' NodeCorpus txt v [])
  pids  <- mkNodeR' (concat $ map (\n -> [childWith uid pid' n]) ns)
  pure $ NewNode pid' pids

postNode uid pid (Node' NodeAnnuaire txt v ns) = do
  NewNode pid' _ <- postNode uid pid (Node' NodeAnnuaire txt v [])
  pids  <- mkNodeR' (concat $ map (\n -> [childWith uid pid' n]) ns)
  pure $ NewNode pid' pids
postNode _ _ (Node' _ _ _ _) = panic "TODO: postNode for this type not implemented yet"


childWith :: UserId -> ParentId -> Node' -> NodeWriteT
childWith uId pId (Node' NodeDocument txt v []) = node2table uId (Just pId) (Node' NodeDocument txt v [])
childWith uId pId (Node' NodeContact  txt v []) = node2table uId (Just pId) (Node' NodeContact txt v [])
childWith _   _   (Node' _        _   _ _) = panic "This NodeType can not be a child"



mk :: Connection -> NodeType -> Maybe ParentId -> Text -> IO [Int]
mk c nt pId name  = mk' c nt userId pId name
  where
    userId = 1

mk' :: Connection -> NodeType -> UserId -> Maybe ParentId -> Text -> IO [Int]
mk' c nt uId pId name  = map fromIntegral <$> insertNodesWithParentR pId [node nt name hd pId uId] c
  where
    hd = HyperdataUser . Just . pack $ show EN

type Name = Text

mk'' :: NodeType -> Maybe ParentId -> UserId -> Name -> Cmd [Int]
mk'' NodeUser Nothing uId name  = mkCmd $ \c -> mk' c NodeUser uId Nothing name
mk'' NodeUser _       _   _     = panic "NodeUser do not have any parent"
mk'' _        Nothing _   _     = panic "NodeType does   have a   parent"
mk'' nt       pId     uId name  = mkCmd $ \c -> mk' c nt uId pId name


mkRoot :: Username -> UserId -> Cmd [Int]
mkRoot uname uId = case uId > 0 of
               False -> panic "UserId <= 0"
               True  -> mk'' NodeUser Nothing uId uname

mkCorpus :: Maybe Name -> Maybe HyperdataCorpus -> ParentId -> UserId -> Cmd [Int]
mkCorpus n h p u = insertNodesR' [nodeCorpusW n h p u]

--{-
getOrMkList :: ParentId -> UserId -> Cmd Int
getOrMkList pId uId = do
  maybeList <- defaultListSafe' pId
  case maybeList of
    Nothing -> maybe (panic "no list") identity <$> headMay <$> mkList pId uId
    Just x  -> pure x

defaultListSafe' :: CorpusId -> Cmd (Maybe ListId)
defaultListSafe' cId = mkCmd $ \c -> do
  maybeNode <- headMay <$> getListsWithParentId c cId
  case maybeNode of
    Nothing -> pure Nothing
    (Just node) -> pure $ Just $ _node_id node
--}

defaultListSafe :: Connection -> CorpusId -> IO (Maybe ListId)
defaultListSafe c cId = do
  maybeNode <- headMay <$> getListsWithParentId c cId
  case maybeNode of
    Nothing -> pure Nothing
    (Just node) -> pure $ Just $ _node_id node

defaultList :: Connection -> CorpusId -> IO ListId
defaultList c cId = maybe (panic errMessage) identity <$> defaultListSafe c cId
  where
    errMessage = "Gargantext.API.Ngrams.defaultList: no list found"

mkList :: ParentId -> UserId -> Cmd [Int]
mkList p u = insertNodesR' [nodeListW Nothing Nothing p u]

mkGraph :: ParentId -> UserId -> Cmd [Int]
mkGraph p u = insertNodesR' [nodeGraphW Nothing Nothing p u]

mkDashboard :: ParentId -> UserId -> Cmd [Int]
mkDashboard p u = insertNodesR' [nodeDashboardW Nothing Nothing p u]

mkAnnuaire :: ParentId -> UserId -> Cmd [Int]
mkAnnuaire p u = insertNodesR' [nodeAnnuaireW Nothing Nothing p u]

-- | Default CorpusId Master and ListId Master

