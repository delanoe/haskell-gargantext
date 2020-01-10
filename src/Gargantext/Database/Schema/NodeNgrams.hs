{-|
Module      : Gargantext.Database.Schema.NodeNgrams
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

NodeNgrams register Context of Ngrams (named Cgrams then)


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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.UniqueStrict (sortUniq)
import qualified Data.List as List
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as PGS (Query, Only(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
-- import Control.Lens.TH (makeLenses)
import Data.Maybe (Maybe, fromMaybe)
import Gargantext.Core.Types
import Gargantext.Database.Utils
import Gargantext.Database.Schema.Ngrams (NgramsType, ngramsTypeId, fromNgramsTypeId)
import Gargantext.Prelude

type NodeNgramsId = Int

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
                              } deriving (Show, Eq, Ord)

{-
instance ( Eq id
         , Eq node_id'
         , Eq node_subtype
         , Eq ngrams_id
         , Eq ngrams_type
         , Eq ngrams_field
         , Eq ngrams_tag
         , Eq ngrams_class
         , Eq weight
         ) => Eq (NodeNgramsPoly id node_id' node_subtype ngrams_id ngrams_type ngrams_field ngrams_tag ngrams_class weight) where
  (==) (NodeNgrams a b c d e f g h i)
       (NodeNgrams a' b' c' d' e' f' g' h' i') =
         all identity [ a == a'
                       , b == b'
                       , c == c'
                       , d == d'
                       , e == e'
                       , f == f'
                       , g == g'
                       , h == h'
                       , i == i'
                       ]

instance ( Ord id
         , Ord node_id'
         , Ord node_subtype
         , Ord ngrams_id
         , Ord ngrams_type
         , Ord ngrams_field
         , Ord ngrams_tag
         , Ord ngrams_class
         , Ord weight
         ) => Ord (NodeNgramsPoly id node_id' node_subtype ngrams_id ngrams_type ngrams_field ngrams_tag ngrams_class weight) where
  compare (NodeNgrams a _b _c _d _e _f _g _h _i)
          (NodeNgrams a' _b' _c' _d' _e' _f' _g' _h' _i') =
            compare a a'


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

data Returning = Returning { re_type :: Maybe NgramsType
                           , re_terms :: Text
                           , re_ngrams_id :: Int
                           }
  deriving (Show)

instance FromRow Returning where
  fromRow = Returning <$> (fromNgramsTypeId <$> field) <*> field <*> field

getCgramsId :: Map NgramsType (Map Text Int) -> NgramsType -> Text -> Maybe Int
getCgramsId mapId nt t = case Map.lookup nt mapId of
  Nothing     -> Nothing
  Just mapId' -> Map.lookup t mapId'


-- insertDb :: ListId -> Map NgramsType [NgramsElement] -> Cmd err [Result]
listInsertDb :: Show a => ListId
             -> (ListId -> a -> [NodeNgramsW])
             -> a
             -- -> Cmd err [Returning]
             -> Cmd err (Map NgramsType (Map Text Int))
listInsertDb l f ngs = Map.map Map.fromList
                    <$> Map.fromListWith (<>)
                    <$> List.map (\(Returning t tx id) -> (fromJust t, [(tx, id)]))
                    <$> List.filter (\(Returning t _ _) -> isJust t)
                    <$> insertNodeNgrams (f l ngs)

-- TODO optimize with size of ngrams
insertNodeNgrams :: [NodeNgramsW] -> Cmd err [Returning]
insertNodeNgrams nns = runPGSQuery query (PGS.Only $ Values fields nns')
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","int4","text","int4"
                                                      ,"int4","int4","int4","int4"
                                                      ,"float8"]
    -- nns' :: [(Int, ListTypeId, NgramsText, NgramsTypeId ,NgramsField, NgramsTag, NgramsClass, Double)]
    nns' = map (\(NodeNgrams _id (NodeId node_id'') node_subtype ngrams_terms ngrams_type ngrams_field ngrams_tag ngrams_class weight)
                              -> [ toField node_id''
                                 , toField $ listTypeId node_subtype
                                 , toField $ ngrams_terms
                                 , toField $ ngramsTypeId ngrams_type
                                 , toField $ fromMaybe 0 ngrams_field
                                 , toField $ fromMaybe 0 ngrams_tag
                                 , toField $ fromMaybe 0 ngrams_class
                                 , toField weight
                                 ]
                  ) $ sortUniq nns

    query :: PGS.Query
    query = [sql|
          WITH input(node_id, node_subtype, ngrams_terms, ngrams_type, ngrams_field, ngrams_tag, ngrams_class, weight) AS (?),
          return(id, ngrams_type, ngrams_id) AS (
            INSERT INTO node_ngrams (node_id, node_subtype, ngrams_id, ngrams_type, ngrams_field, ngrams_tag, ngrams_class, weight)
            SELECT i.node_id, i.node_subtype, ng.id, i.ngrams_type, i.ngrams_field, i.ngrams_tag, i.ngrams_class, i.weight FROM input as i
            INNER JOIN ngrams as ng ON ng.terms = i.ngrams_terms
            ON CONFLICT(node_id, node_subtype, ngrams_id)
            DO UPDATE SET node_subtype = excluded.node_subtype, ngrams_type = excluded.ngrams_type, ngrams_field = excluded.ngrams_field, ngrams_tag = excluded.ngrams_tag, ngrams_class = excluded.ngrams_class, weight = excluded.weight
            RETURNING id, ngrams_type, ngrams_id
          )
          SELECT return.ngrams_type, ng.terms, return.id FROM return
          INNER JOIN ngrams ng ON return.ngrams_id = ng.id;
  |]
