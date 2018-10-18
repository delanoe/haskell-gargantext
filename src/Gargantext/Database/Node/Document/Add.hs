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
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------
module Gargantext.Database.Node.Document.Add where

import Control.Lens (set)

import Data.Aeson (toJSON, Value)
import Data.ByteString.Internal (ByteString)
import Data.Maybe (maybe)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Connection, FromRow, Query, formatQuery, query, Only(..))
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))

import Data.Text (Text)
import qualified Data.Text                   as DT (pack, unpack, concat)
import qualified Data.Digest.Pure.SHA        as SHA (sha256, showDigest)
import qualified Data.ByteString.Lazy.Char8  as DC (pack)

import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Node (mkCmd, Cmd(..))
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
inputSqlTypes = map DT.pack ["int4","int4","bool"]

-- | SQL query to add documents
-- TODO return id of added documents only
queryAdd :: Query
queryAdd = [sql|
       WITH input_rows(node1_id,node2_id, favorite) AS (?)
       INSERT INTO nodes_nodes (node1_id, node2_id, favorite)
       SELECT * FROM input_rows
       ON CONFLICT (node1_id, node2_id) DO NOTHING -- on unique index
       RETURNING 1
       ;
           |]

prepare :: ParentId -> [NodeId] -> [InputData]
prepare pId ns = map (\nId -> InputData pId nId False) ns

------------------------------------------------------------------------
-- * Main Types used


data InputData = InputData { inNode1_id :: NodeId
                           , inNode2_id :: NodeId
                           , inNode_fav :: Bool
                           } deriving (Show, Generic, Typeable)

instance ToRow InputData where
  toRow inputData = [ toField (inNode1_id inputData)
                    , toField (inNode2_id inputData)
                    , toField (inNode_fav inputData)
                    ]

