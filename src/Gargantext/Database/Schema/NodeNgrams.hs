{-|
Module      : Gargantext.Database.Schema.NodeNgrams
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

NodeNgrams: mainly NodeList and its ngrams.

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNgrams where

import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
-- import Control.Lens.TH (makeLenses)
import Data.Maybe (Maybe, fromMaybe)
import Gargantext.Core.Types
import Gargantext.Database.Utils
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Schema.Ngrams (NgramsType, NgramsTypeId, ngramsTypeId)
import Gargantext.Prelude

data NodeNgramsPoly id
                    node_id'
                    node_subtype
                    ngrams_id
                    ngrams_type
                    ngrams_field
                    ngrams_tag
                    ngrams_class
                    weight
                   = NodeNgrams { _nng_id        :: id
                                , _nng_node_id   :: node_id'
                                , _nng_node_subtype :: node_subtype
                                , _nng_ngrams_id :: ngrams_id
                                , _nng_ngrams_type :: ngrams_type
                                , _nng_ngrams_field :: ngrams_field
                                , _nng_ngrams_tag :: ngrams_tag
                                , _nng_ngrams_class :: ngrams_class
                                , _nng_ngrams_weight :: weight
                              } deriving (Show)

{-
type NodeNgramsWrite = NodeNgramsPoly (Maybe (Column (PGInt4)))
                                      (Column (PGInt4))
                                      (Maybe  (Column (PGInt4)))
                                      (Column (PGInt4))
                                      (Maybe  (Column (PGInt4)))
                                      (Maybe  (Column (PGInt4)))
                                      (Maybe  (Column (PGInt4)))
                                      (Maybe  (Column (PGInt4)))
                                      (Maybe  (Column (PGFloat8)))

type NodeNodeRead    = NodeNgramsPoly (Column PGInt4)
                                      (Column PGInt4)
                                      (Column PGInt4)
                                      (Column PGInt4)
                                      (Column PGInt4)
                                      (Column PGInt4)
                                      (Column PGInt4)
                                      (Column PGInt4)
                                      (Column PGFloat8)

type NodeNgramsReadNull = NodeNgramsPoly (Column (Nullable PGInt4))
                                         (Column (Nullable PGInt4))
                                         (Column (Nullable PGInt4))
                                         (Column (Nullable PGInt4))

                                         (Column (Nullable PGInt4))
                                         (Column (Nullable PGInt4))
                                         (Column (Nullable PGInt4))
                                         (Column (Nullable PGInt4))
                                         (Column (Nullable PGFloat8))
-}
type NgramsId = Int
type NgramsField = Int
type NgramsTag   = Int
type NgramsClass = Int
type NgramsText  = Text

-- Example of list Ngrams
-- type ListNgrams = NodeNgramsPoly (Maybe Int) ListType Text 

type NodeNgramsW =
  NodeNgramsPoly (Maybe Int) NodeId ListType NgramsText
                  NgramsType (Maybe NgramsField) (Maybe NgramsTag) (Maybe NgramsClass)
                  Double

data Result = Result { unResult :: Int }
  deriving (Show)

instance FromRow Result where
  fromRow = Result <$> field

-- insertDb :: ListId -> Map NgramsType [NgramsElemet] -> Cmd err [Result]
listInsertDb :: ListId
             -> (ListId -> a -> [NodeNgramsW])
             -> a
             -> Cmd err [Result]
listInsertDb l f ngs = insertNodeNgrams (f l ngs)

-- TODO optimize with size of ngrams
insertNodeNgrams :: [NodeNgramsW] -> Cmd err [Result]
insertNodeNgrams nns = runPGSQuery query (PGS.Only $ Values fields nns')
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) [ "int4","int4","text","int4"
                                                      ,"int4","int4","int4","int4"
                                                      ,"float8"]
    nns' :: [(Int, ListTypeId, NgramsText, NgramsTypeId ,NgramsField, NgramsTag, NgramsClass, Double)]
    nns' = map (\(NodeNgrams _id (NodeId node_id'') node_subtype ngrams_terms ngrams_type ngrams_field ngrams_tag ngrams_class weight)
                              -> ( node_id''
                                 , listTypeId node_subtype
                                 , ngrams_terms
                                 , ngramsTypeId ngrams_type
                                 , fromMaybe 0 ngrams_field
                                 , fromMaybe 0 ngrams_tag
                                 , fromMaybe 0 ngrams_class
                                 , weight
                                 )
                  ) nns

    query :: PGS.Query
    query = [sql|
          INSERT INTO node_ngrams_ngrams VALUES (node_id, node_type, ngrams_id, ngrams_type, ngrams_field, ngrams_tag, ngrams_class, weight)
          SELECT n.node_id, n.node_type, ng.ngrams_id, n.ngrams_type, n.ngrams_field, n.ngrams_tag, n.ngrams_class, n.weight FROM (?)
              AS n(node_id, node_type, ngrams_terms, ngrams_type, ngrams_field, ngrams_tag, ngrams_class, weight)
          INNER JOIN ngrams as ng ON ng.terms = n.ngrams_terms
          ON CONFLICT(node_id, ngrams_id)
          DO UPDATE SET node_type = excluded.node_type, ngrams_type = excluded.ngrams_type, ngrams_field = excluded.ngrams_field, ngrams_tag = excluded.ngrams_tag, ngrams_class = excluded.ngrams_class, weight = excluded.weight
  |]
