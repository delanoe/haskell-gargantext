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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.Database.Node where

import Database.PostgreSQL.Simple.FromField ( Conversion
                                            , ResultError(ConversionFailed)
                                            , FromField
                                            , fromField
                                            , returnError
                                            )
import Prelude hiding (null, id, map, sum)

import Gargantext.Types
import Gargantext.Types.Main (NodeType)
import Gargantext.Database.Queries
import Gargantext.Prelude hiding (sum)


import Database.PostgreSQL.Simple.Internal  (Field)
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson
import Data.Maybe (Maybe, fromMaybe)
import Data.Text (Text)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Internal as DBI
import Database.PostgreSQL.Simple (Connection)
import Opaleye

-- | Types for Node Database Management
data PGTSVector


instance FromField HyperdataCorpus where
    fromField = fromField'

instance FromField HyperdataDocument where
    fromField = fromField'

instance FromField HyperdataProject where
    fromField = fromField'

instance FromField HyperdataUser where
    fromField = fromField'


instance QueryRunnerColumnDefault PGJsonb HyperdataDocument where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataProject  where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataUser     where
  queryRunnerColumnDefault = fieldQueryRunnerColumn



fromField' :: (Typeable b, FromJSON b) => Field -> Maybe DBI.ByteString -> Conversion b
fromField' field mb = do
    v <- fromField field mb
    valueToHyperdata v
      where
          valueToHyperdata v = case fromJSON v of
             Success a  -> pure a
             Error _err -> returnError ConversionFailed field "cannot parse hyperdata"


$(makeAdaptorAndInstance "pNode" ''NodePoly)
$(makeLensesWith abbreviatedFields   ''NodePoly)


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


queryNodeTable :: Query NodeRead
queryNodeTable = queryTable nodeTable


selectNodes :: Column PGInt4 -> Query NodeRead
selectNodes id = proc () -> do
    row <- queryNodeTable -< ()
    restrict -< node_id row .== id
    returnA -< row

runGetNodes :: Connection -> Query NodeRead -> IO [Node Value]
runGetNodes = runQuery

-- | order by publication date
-- Favorites (Bool), node_ngrams
selectNodesWith :: ParentId -> Maybe NodeType -> Maybe Offset -> Maybe Limit -> Query NodeRead
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
deleteNode conn n = fromIntegral 
                 <$> runDelete conn nodeTable 
                 (\(Node n_id _ _ _ _ _ _) -> n_id .== pgInt4 n)

deleteNodes :: Connection -> [Int] -> IO Int
deleteNodes conn ns = fromIntegral 
                   <$> runDelete conn nodeTable 
                   (\(Node n_id _ _ _ _ _ _) -> in_ ((map pgInt4 ns)) n_id)


getNodesWith :: Connection -> Int -> Maybe NodeType -> Maybe Offset -> Maybe Limit -> IO [Node Value]
getNodesWith conn parentId nodeType maybeOffset maybeLimit = 
    runQuery conn $ selectNodesWith 
                  parentId nodeType maybeOffset maybeLimit




-- NP check type
getNodesWithParentId :: Connection -> Int -> Maybe Text -> IO [Node Value]
getNodesWithParentId conn n _ = runQuery conn $ selectNodesWithParentID n

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
    fromMaybe (error "TODO: 404") . headMay <$> runQuery conn (limit 1 $ selectNodes (pgInt4 id))

getNodesWithType :: Connection -> Column PGInt4 -> IO [Node Value]
getNodesWithType conn type_id = do
    runQuery conn $ selectNodesWithType type_id


