{-|
Module      : Gargantext.Database.Node.Document.Add
Description : Importing context of texts (documents)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Add Documents/Contact to a Corpus/Annuaire.
 
-}
------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------
module Gargantext.Database.Node.Document.Add where


import Data.ByteString.Internal (ByteString)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Query, formatQuery, query, Only(..))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))

import Data.Text (Text)

import Gargantext.Database.Schema.Node (mkCmd, Cmd(..))
import Gargantext.Database.Types.Node
import Gargantext.Prelude

import GHC.Generics (Generic)
---------------------------------------------------------------------------

type ParentId = Int

add :: ParentId -> [NodeId] -> Cmd [Only Int]
add pId ns = mkCmd $ \c -> query c queryAdd (Only $ Values fields inputData)
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes
    inputData = prepare pId ns

add_debug :: ParentId -> [NodeId] -> Cmd ByteString
add_debug pId ns = mkCmd $ \c -> formatQuery c queryAdd (Only $ Values fields inputData)
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes
    inputData = prepare pId ns



-- | Input Tables: types of the tables
inputSqlTypes :: [Text]
inputSqlTypes = ["int4","int4","bool","bool"]

-- | SQL query to add documents
-- TODO return id of added documents only
queryAdd :: Query
queryAdd = [sql|
       WITH input_rows(node1_id,node2_id, favorite, delete) AS (?)
       INSERT INTO nodes_nodes (node1_id, node2_id, favorite, delete)
       SELECT * FROM input_rows
       ON CONFLICT (node1_id, node2_id) DO NOTHING -- on unique index
       RETURNING 1
       ;
           |]

prepare :: ParentId -> [NodeId] -> [InputData]
prepare pId ns = map (\nId -> InputData pId nId False False) ns

------------------------------------------------------------------------
-- * Main Types used


data InputData = InputData { inNode1_id :: NodeId
                           , inNode2_id :: NodeId
                           , inNode_fav :: Bool
                           , inNode_del :: Bool
                           } deriving (Show, Generic, Typeable)

instance ToRow InputData where
  toRow inputData = [ toField (inNode1_id inputData)
                    , toField (inNode2_id inputData)
                    , toField (inNode_fav inputData)
                    , toField (inNode_del inputData)
                    ]

