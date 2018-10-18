{-|
Module      : Gargantext.Database.Node
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
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Node where

import Data.Text (pack)
import GHC.Int (Int64)
import Control.Lens (set)
import Data.Maybe
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField ( Conversion
                                            , ResultError(ConversionFailed)
                                            , FromField
                                            , fromField
                                            , returnError
                                            )
import Prelude hiding (null, id, map, sum)

import Gargantext.Core (Lang(..))
import Gargantext.Core.Types
import Gargantext.Database.Types.Node (NodeType)
import Gargantext.Database.Queries
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Prelude hiding (sum)

import Database.PostgreSQL.Simple.Internal  (Field)
import Control.Applicative (Applicative)
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Maybe (Maybe, fromMaybe)
import Data.Text (Text)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Typeable (Typeable)

import qualified Data.ByteString      as DB
import qualified Data.ByteString.Lazy as DBL
import Data.ByteString (ByteString)

import Database.PostgreSQL.Simple (Connection)
import Opaleye hiding (FromField)
import Opaleye.Internal.QueryArr (Query)
import qualified Data.Profunctor.Product as PP

------------------------------------------------------------------------
{- | Reader Monad reinvented here:

newtype Cmd a = Cmd { unCmd :: Connection -> IO a }

instance Monad Cmd where
  return a = Cmd $ \_ -> return a

  m >>= f = Cmd $ \c -> do
    a <- unCmd m c
    unCmd (f a) c
-}
newtype Cmd a = Cmd (ReaderT Connection IO a)
  deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO)

runCmd :: Connection -> Cmd a -> IO a
runCmd c (Cmd f) = runReaderT f c

mkCmd :: (Connection -> IO a) -> Cmd a
mkCmd = Cmd . ReaderT

------------------------------------------------------------------------
type CorpusId = Int
type AnnuaireId = Int
type UserId = NodeId
type TypeId = Int
------------------------------------------------------------------------
instance FromField HyperdataCorpus where
    fromField = fromField'

instance FromField HyperdataDocument where
    fromField = fromField'

instance FromField HyperdataDocumentV3 where
    fromField = fromField'

instance FromField HyperdataUser where
    fromField = fromField'
------------------------------------------------------------------------
instance QueryRunnerColumnDefault PGJsonb HyperdataDocument where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocumentV3 where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataUser     where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
------------------------------------------------------------------------

fromField' :: (Typeable b, FromJSON b) => Field -> Maybe DB.ByteString -> Conversion b
fromField' field mb = do
    v <- fromField field mb
    valueToHyperdata v
      where
          valueToHyperdata v = case fromJSON v of
             Success a  -> pure a
             Error _err -> returnError ConversionFailed field "cannot parse hyperdata"


$(makeAdaptorAndInstance "pNode" ''NodePoly)
$(makeLensesWith abbreviatedFields ''NodePoly)


nodeTable :: Table NodeWrite NodeRead
nodeTable = Table "nodes" (pNode Node { _node_id                = optional "id"
                                        , _node_typename        = required "typename"
                                        , _node_userId          = required "user_id"
                                        , _node_parentId        = required "parent_id"
                                        , _node_name            = required "name"
                                        , _node_date            = optional "date"
                                        , _node_hyperdata       = required "hyperdata"
                     --                   , node_titleAbstract   = optional "title_abstract"
                                        }
                            )


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


queryNodeTable :: Query NodeRead
queryNodeTable = queryTable nodeTable

selectNode :: Column PGInt4 -> Query NodeRead
selectNode id = proc () -> do
    row <- queryNodeTable -< ()
    restrict -< _node_id row .== id
    returnA -< row

runGetNodes :: Query NodeRead -> Cmd [Node Value]
runGetNodes q = mkCmd $ \conn -> runQuery conn q

------------------------------------------------------------------------
selectRootUser :: UserId -> Query NodeRead
selectRootUser userId = proc () -> do
    row <- queryNodeTable -< ()
    restrict -< _node_userId   row .== (pgInt4 userId)
    restrict -< _node_typename row .== (pgInt4 $ nodeTypeId NodeUser)
    returnA -< row

getRoot :: UserId -> Cmd [Node HyperdataUser]
getRoot userId = mkCmd $ \conn -> runQuery conn (selectRootUser userId)
------------------------------------------------------------------------

-- | order by publication date
-- Favorites (Bool), node_ngrams
selectNodesWith :: ParentId     -> Maybe NodeType
                -> Maybe Offset -> Maybe Limit   -> Query NodeRead
selectNodesWith parentId maybeNodeType maybeOffset maybeLimit = 
        --offset' maybeOffset $ limit' maybeLimit $ orderBy (asc (hyperdataDocument_Publication_date . node_hyperdata)) $ selectNodesWith' parentId typeId
        limit' maybeLimit $ offset' maybeOffset $ orderBy (asc _node_id) $ selectNodesWith' parentId maybeNodeType

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


--type Cmd' a = forall m. (MonadReader env m, HasConnection env, MonadIO m) => m a


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


getNodesWith :: Connection   -> Int         -> Maybe NodeType 
             -> Maybe Offset -> Maybe Limit -> IO [Node Value]
getNodesWith conn parentId nodeType maybeOffset maybeLimit = 
    runQuery conn $ selectNodesWith 
                  parentId nodeType maybeOffset maybeLimit


-- NP check type
getNodesWithParentId :: Int
                     -> Maybe Text -> Connection -> IO [Node Value]
getNodesWithParentId n _ conn = runQuery conn $ selectNodesWithParentID n

getNodesWithParentId' :: Int
                     -> Maybe Text -> Connection -> IO [Node Value]
getNodesWithParentId' n _ conn = runQuery conn $ selectNodesWithParentID n


------------------------------------------------------------------------
getDocumentsV3WithParentId :: Connection -> Int -> IO [Node HyperdataDocumentV3]
getDocumentsV3WithParentId conn n = runQuery conn $ selectNodesWith' n (Just NodeDocument)

getDocumentsWithParentId :: Connection -> Int -> IO [Node HyperdataDocument]
getDocumentsWithParentId conn n = runQuery conn $ selectNodesWith' n (Just NodeDocument)

------------------------------------------------------------------------


selectNodesWithParentID :: Int -> Query NodeRead
selectNodesWithParentID n = proc () -> do
    row@(Node _ _ _ parent_id _ _ _) <- queryNodeTable -< ()
    restrict -< if n > 0
                   then
                        parent_id .== (toNullable $ pgInt4 n)
                   else
                        isNull parent_id
    returnA -< row


selectNodesWithType :: Column PGInt4 -> Query NodeRead
selectNodesWithType type_id = proc () -> do
    row@(Node _ tn _ _ _ _ _) <- queryNodeTable -< ()
    restrict -< tn .== type_id
    returnA -< row


getNode :: Connection -> Int -> IO (Node Value)
getNode conn id = do
    fromMaybe (error $ "Node does node exist: " <> show id) . headMay <$> runQuery conn (limit 1 $ selectNode (pgInt4 id))


getNodesWithType :: Connection -> Column PGInt4 -> IO [Node HyperdataDocument]
getNodesWithType conn type_id = do
    runQuery conn $ selectNodesWithType type_id


------------------------------------------------------------------------
-- WIP
-- TODO Classe HasDefault where
-- default NodeType = Hyperdata
------------------------------------------------------------------------
type NodeWrite' = NodePoly (Maybe Int) Int Int (Maybe ParentId) Text (Maybe UTCTime) ByteString
------------------------------------------------------------------------
defaultUser :: HyperdataUser
defaultUser = HyperdataUser (Just $ (pack . show) EN)

nodeUserW :: Maybe Name -> Maybe HyperdataUser -> UserId -> NodeWrite'
nodeUserW maybeName maybeHyperdata = node NodeUser name (Hyperdata user) Nothing
  where
    name = maybe "User" identity maybeName
    user = maybe defaultUser identity maybeHyperdata
------------------------------------------------------------------------
defaultFolder :: HyperdataFolder
defaultFolder = HyperdataFolder (Just "Markdown Description")

nodeFolderW :: Maybe Name -> Maybe HyperdataFolder -> ParentId -> UserId -> NodeWrite'
nodeFolderW maybeName maybeFolder pid = node NodeFolder name (Hyperdata folder) (Just pid)
  where
    name   = maybe "Folder" identity maybeName
    folder = maybe defaultFolder identity maybeFolder
------------------------------------------------------------------------
defaultCorpus :: HyperdataCorpus
defaultCorpus = (HyperdataCorpus (Just "Title") (Just "Descr") (Just "Bool query") (Just "Authors") Nothing)

nodeCorpusW :: Maybe Name -> Maybe HyperdataCorpus -> ParentId -> UserId -> NodeWrite'
nodeCorpusW maybeName maybeCorpus pId = node NodeCorpus name (Hyperdata corpus) (Just pId)
  where
    name   = maybe "Corpus" identity maybeName
    corpus = maybe defaultCorpus identity maybeCorpus
                   --------------------------
defaultDocument :: HyperdataDocument
defaultDocument = hyperdataDocument

nodeDocumentW :: Maybe Name -> Maybe HyperdataDocument -> CorpusId -> UserId -> NodeWrite'
nodeDocumentW maybeName maybeDocument cId = node NodeDocument name (Hyperdata doc) (Just cId)
  where
    name = maybe "Document" identity maybeName
    doc  = maybe defaultDocument identity maybeDocument
------------------------------------------------------------------------
defaultAnnuaire :: HyperdataAnnuaire
defaultAnnuaire = HyperdataAnnuaire (Just "Title") (Just "Description")

nodeAnnuaireW :: Maybe Name -> Maybe HyperdataAnnuaire -> ParentId -> UserId -> NodeWrite'
nodeAnnuaireW maybeName maybeAnnuaire pId = node NodeAnnuaire name (Hyperdata annuaire) (Just pId)
  where
    name     = maybe "Annuaire" identity maybeName
    annuaire = maybe defaultAnnuaire identity maybeAnnuaire
                   --------------------------
defaultContact :: HyperdataContact
defaultContact = HyperdataContact (Just "Name") (Just "email@here")

nodeContactW :: Maybe Name -> Maybe HyperdataContact -> AnnuaireId -> UserId -> NodeWrite'
nodeContactW maybeName maybeContact aId = node NodeContact name (Hyperdata contact) (Just aId)
  where
    name    = maybe "Contact" identity maybeName
    contact = maybe defaultContact identity maybeContact
------------------------------------------------------------------------
------------------------------------------------------------------------
node :: ToJSON a => NodeType -> Name -> Hyperdata a -> Maybe ParentId -> UserId -> NodeWrite'
node nodeType name hyperData parentId userId = Node Nothing typeId userId parentId name Nothing byteData
  where
    typeId = nodeTypeId nodeType
    byteData = DB.pack $ DBL.unpack $ encode $ unHyperdata hyperData

                  -------------------------------
node2row :: (Functor maybe1, Functor maybe2, Functor maybe3) =>
              NodePoly (maybe2 Int)                      Int            Int  (maybe1           Int) 
                                Text        (maybe3 UTCTime)             ByteString
                    -> (maybe2 (Column PGInt4), Column PGInt4, Column PGInt4, maybe1 (Column PGInt4)
                     , Column PGText, maybe3 (Column PGTimestamptz), Column PGJsonb)
node2row (Node id tn ud pid nm dt hp) = ((pgInt4    <$> id)
                                        ,(pgInt4        tn)
                                        ,(pgInt4        ud)
                                        ,(pgInt4   <$> pid)
                                        ,(pgStrictText  nm)
                                        ,(pgUTCTime <$> dt)
                                        ,(pgStrictJSONB hp)
                                        )
------------------------------------------------------------------------
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


type NodeWriteT =  ( Maybe (Column PGInt4)
                   ,        Column PGInt4
                   ,        Column PGInt4
                   , Maybe (Column PGInt4)
                   ,        Column PGText
                   , Maybe (Column PGTimestamptz)
                   ,        Column PGJsonb
                   )


mkNode' :: [NodeWriteT] -> Cmd Int64
mkNode' ns = mkCmd $ \conn -> runInsertMany conn nodeTable' ns

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


-- TODO: remove hardcoded userId (with Reader)
-- TODO: user Reader in the API and adapt this function
userId = 1

mk :: Connection -> NodeType -> Maybe ParentId -> Text -> IO [Int]
mk c nt pId name  = mk' c nt userId pId name

mk' :: Connection -> NodeType -> UserId -> Maybe ParentId -> Text -> IO [Int]
mk' c nt uId pId name  = map fromIntegral <$> insertNodesWithParentR pId [node nt name hd pId uId] c
  where
    hd = Hyperdata (HyperdataUser (Just $ (pack . show) EN))

type Name = Text

mk'' :: NodeType -> Maybe ParentId -> UserId -> Name -> Cmd [Int]
mk'' NodeUser Nothing uId name  = mkCmd $ \c -> mk' c NodeUser uId Nothing name
mk'' NodeUser _       _   _     = panic "NodeUser can not has a parent"
mk'' _        Nothing _   _     = panic "NodeType needs a parent"
mk'' nt       pId     uId name  = mkCmd $ \c -> mk' c nt uId pId name

mkRoot :: UserId -> Cmd [Int]
mkRoot uId = case uId > 0 of
               False -> panic "UserId <= 0"
               True  -> mk'' NodeUser Nothing uId ("User Node : " <> (pack . show) uId)
