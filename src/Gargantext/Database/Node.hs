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
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Node where


import GHC.Int (Int64)
import Data.Maybe
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField ( Conversion
                                            , ResultError(ConversionFailed)
                                            , FromField
                                            , fromField
                                            , returnError
                                            )
import Prelude hiding (null, id, map, sum)

import Gargantext.Core.Types
import Gargantext.Core.Types.Node (NodeType)
import Gargantext.Database.Queries
import Gargantext.Prelude hiding (sum)


import Database.PostgreSQL.Simple.Internal  (Field)
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson
import Data.Maybe (Maybe, fromMaybe)
import Data.Text (Text, pack)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Typeable (Typeable)

import qualified Data.ByteString      as DB
import qualified Data.ByteString.Lazy as DBL
import Data.ByteString (ByteString)

import Database.PostgreSQL.Simple (Connection)
import Opaleye hiding (FromField)
import Opaleye.Internal.QueryArr (Query)
import qualified Data.Profunctor.Product as PP
-- | Types for Node Database Management
data PGTSVector


instance FromField HyperdataCorpus where
    fromField = fromField'

instance FromField HyperdataDocument where
    fromField = fromField'

instance FromField HyperdataDocumentV3 where
    fromField = fromField'

instance FromField HyperdataProject where
    fromField = fromField'

instance FromField HyperdataUser where
    fromField = fromField'


instance QueryRunnerColumnDefault PGJsonb HyperdataDocument where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
instance QueryRunnerColumnDefault PGJsonb HyperdataDocumentV3 where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataProject  where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataUser     where
  queryRunnerColumnDefault = fieldQueryRunnerColumn



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
nodeTable = Table "nodes" (pNode Node { node_id                = optional "id"
                                        , node_typename        = required "typename"
                                        , node_userId          = required "user_id"
                                        , node_parentId        = required "parent_id"
                                        , node_name            = required "name"
                                        , node_date            = optional "date"
                                        , node_hyperdata       = required "hyperdata"
                     --                   , node_titleAbstract   = optional "title_abstract"
                                        }
                            )


nodeTable' :: Table (Maybe (Column PGInt4)
                    ,       Column PGInt4
                    ,       Column PGInt4
                    ,       Column PGInt4
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
                               , required "parent_id"
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
    restrict -< node_id row .== id
    returnA -< row

runGetNodes :: Connection -> Query NodeRead -> IO [Node Value]
runGetNodes = runQuery

-- | order by publication date
-- Favorites (Bool), node_ngrams
selectNodesWith :: ParentId     -> Maybe NodeType
                -> Maybe Offset -> Maybe Limit   -> Query NodeRead
selectNodesWith parentId maybeNodeType maybeOffset maybeLimit = 
        --offset' maybeOffset $ limit' maybeLimit $ orderBy (asc (hyperdataDocument_Publication_date . node_hyperdata)) $ selectNodesWith' parentId typeId
        limit' maybeLimit $ offset' maybeOffset $ orderBy (asc node_id) $ selectNodesWith' parentId maybeNodeType

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



deleteNode :: Connection -> Int -> IO Int
deleteNode conn n = fromIntegral <$> runDelete conn nodeTable
                 (\(Node n_id _ _ _ _ _ _) -> n_id .== pgInt4 n)

deleteNodes :: Connection -> [Int] -> IO Int
deleteNodes conn ns = fromIntegral <$> runDelete conn nodeTable
                   (\(Node n_id _ _ _ _ _ _) -> in_ ((map pgInt4 ns)) n_id)


getNodesWith :: Connection   -> Int         -> Maybe NodeType 
             -> Maybe Offset -> Maybe Limit -> IO [Node Value]
getNodesWith conn parentId nodeType maybeOffset maybeLimit = 
    runQuery conn $ selectNodesWith 
                  parentId nodeType maybeOffset maybeLimit


-- NP check type
getNodesWithParentId :: Connection -> Int 
                     -> Maybe Text -> IO [Node Value]
getNodesWithParentId conn n _ = runQuery conn $ selectNodesWithParentID n

getNodesWithParentId' :: Connection -> Int 
                     -> Maybe Text -> IO [Node Value]
getNodesWithParentId' conn n _ = runQuery conn $ selectNodesWithParentID n

getDocumentsWithParentId :: Connection -> Int -> IO [Node HyperdataDocumentV3]
getDocumentsWithParentId conn n = runQuery conn $ selectNodesWith' n (Just Document)



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

type UserId = NodeId
type TypeId = Int


------------------------------------------------------------------------
-- Quick and dirty
------------------------------------------------------------------------
type NodeWrite' = NodePoly (Maybe Int) Int Int (ParentId) Text (Maybe UTCTime) ByteString

--node :: UserId -> ParentId -> NodeType -> Text -> Value -> NodeWrite'
node :: UserId -> ParentId -> NodeType -> Text -> Value -> NodeWrite'
node userId parentId nodeType name nodeData = Node Nothing typeId userId parentId name Nothing byteData
  where
    typeId = nodeTypeId nodeType
    byteData = DB.pack $ DBL.unpack $ encode nodeData



node2write :: (Functor f2, Functor f1) =>
              Int
              -> NodePoly (f1 Int) Int Int parentId Text (f2 UTCTime) ByteString
              -> (f1 (Column PGInt4), Column PGInt4, Column PGInt4,
                  Column PGInt4, Column PGText, f2 (Column PGTimestamptz),
                  Column PGJsonb)
node2write pid (Node id tn ud _ nm dt hp) = ((pgInt4    <$> id)
                                         ,(pgInt4        tn)
                                         ,(pgInt4        ud)
                                         ,(pgInt4        pid)
                                         ,(pgStrictText  nm)
                                         ,(pgUTCTime <$> dt)
                                         ,(pgStrictJSONB hp)
                                         )


mkNode :: Connection -> ParentId -> [NodeWrite'] -> IO Int64
mkNode conn pid ns = runInsertMany conn nodeTable' $ map (node2write pid) ns

mkNodeR :: Connection -> ParentId -> [NodeWrite'] -> IO [Int]
mkNodeR conn pid ns = runInsertManyReturning conn nodeTable' (map (node2write pid) ns) (\(i,_,_,_,_,_,_) -> i)


------------------------------------------------------------------------
-- TODO Hierachy of Nodes
-- post and get same types Node' and update if changes

{- TODO semantic to achieve
post c uid pid [ Node' Corpus "name" "{}" []
               , Node' Folder "name" "{}" [Node' Corpus "test 2" "" [ Node' Document "title" "metaData" []
                                                                    , Node' Document "title" "jsonData" []
                                                                    ]
                                          ]
               ]
-}
------------------------------------------------------------------------

-- TODO
-- currently this function remove the child relation
-- needs a Temporary type between Node' and NodeWriteT
node2table :: UserId -> ParentId -> Node' -> [NodeWriteT]
node2table uid pid (Node' nt txt v []) = [( Nothing, (pgInt4$ nodeTypeId nt), (pgInt4 uid), (pgInt4 pid)
                                         , pgStrictText txt, Nothing, pgStrictJSONB $ DB.pack $ DBL.unpack $ encode v)]
node2table _ _ (Node' _ _ _ _) = panic $ pack "node2table: should not happen, Tree insert not implemented yet"


data Node' = Node' { _n_type :: NodeType
                   , _n_name :: Text
                   , _n_data :: Value
                   , _n_children :: [Node']
                   } deriving (Show)


type NodeWriteT =  ( Maybe (Column PGInt4)
                   , Column PGInt4, Column PGInt4
                   , Column PGInt4, Column PGText
                   , Maybe (Column PGTimestamptz)
                   , Column PGJsonb
                   )


mkNode' :: Connection -> [NodeWriteT] -> IO Int64
mkNode' conn ns = runInsertMany conn nodeTable' ns

mkNodeR' :: Connection -> [NodeWriteT] -> IO [Int]
mkNodeR' conn ns = runInsertManyReturning conn nodeTable' ns (\(i,_,_,_,_,_,_) -> i)

-- | postNode
postNode :: Connection -> UserId -> ParentId -> Node' -> IO [Int]
postNode c uid pid (Node' nt txt v []) = mkNodeR' c (node2table uid pid (Node' nt txt v []))

postNode c uid pid (Node' NodeCorpus txt v ns) = do
  [pid']  <- postNode c uid pid (Node' NodeCorpus txt v [])
  pids    <- mkNodeR' c $ concat $ map (\n -> childWith uid pid' n) ns
  pure (pids)

postNode c uid pid (Node' Annuaire txt v ns) = do
  [pid']  <- postNode c uid pid (Node' Annuaire txt v [])
  pids    <- mkNodeR' c $ concat $ map (\n -> childWith uid pid' n) ns
  pure (pids)
postNode _ _ _ (Node' _ _ _ _) = panic $ pack "postNode for this type not implemented yet"


childWith :: UserId -> ParentId -> Node' -> [NodeWriteT]
childWith uId pId (Node' Document txt v []) = node2table uId pId (Node' Document txt v [])
childWith uId pId (Node' UserPage txt v []) = node2table uId pId (Node' UserPage txt v [])
childWith _   _   (Node' _        _   _ _) = panic $ pack "This NodeType can not be a child"

