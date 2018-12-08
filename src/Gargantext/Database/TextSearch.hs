{-|
Module      : Gargantext.Database.TextSearch
Description : Postgres text search experimentation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.TextSearch where


import Data.Aeson
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Text (Text, words, unpack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Types.Node (NodeType(..))
import Gargantext.Prelude
import Gargantext.Database.Facet
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.NodeNode
import Gargantext.Database.Schema.NodeNgram
import Gargantext.Database.Queries
import Gargantext.Core.Types
import Control.Arrow (returnA)
import qualified Opaleye as O hiding (Order)
import Opaleye hiding (Query, Order)

newtype TSQuery = UnsafeTSQuery [Text]

globalTextSearch :: Connection -> ParentId -> Text -> IO [(NodeId, HyperdataDocument)]
globalTextSearch c p t = runQuery c (globalTextSearchQuery p t)

-- | Global search query where ParentId is Master Node Corpus Id 
globalTextSearchQuery :: ParentId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
globalTextSearchQuery _ q = proc () -> do
    row <- queryNodeTable -< ()
    restrict -< (_node_search row)    @@ (pgTSQuery (unpack q))
    restrict -< (_node_typename row) .== (pgInt4 $ nodeTypeId NodeDocument)
    returnA  -< (_node_id row, _node_hyperdata row)

------------------------------------------------------------------------
{-
graphCorpusAuthorQuery :: O.Query (NodeRead, (NodeNgramRead, (NgramsReadNull, NodeNgramReadNull)))
graphCorpusAuthorQuery = leftJoin4 queryNgramsTable queryNodeNgramTable queryNodeNgramTable queryNodeTable cond12 cond23 cond34
  where
    --cond12 :: (NgramsRead, NodeNgramRead) -> Column PGBool
    cond12 = undefined

    cond23 :: (NodeNgramRead, (NodeNgramRead, NodeNgramReadNull)) -> Column PGBool
    cond23 = undefined
    
    cond34 :: (NodeRead, (NodeNgramRead, (NodeReadNull, NodeNgramReadNull))) -> Column PGBool
    cond34 = undefined
--}
--runGraphCorpusDocSearch :: Connection -> CorpusId -> Text -> IO [(Column PGInt4, Column PGJsonb)]
--runGraphCorpusDocSearch c cId t = runQuery c $ graphCorpusDocSearch cId t


-- | todo add limit and offset and order
graphCorpusDocSearch :: CorpusId -> Text -> O.Query (Column PGInt4, Column PGJsonb)
graphCorpusDocSearch cId t = proc () -> do
  (n, nn) <- graphCorpusDocSearchQuery -< ()
  restrict -< (_node_search n) @@ (pgTSQuery (unpack t))
  restrict -< ( nodeNode_node1_id nn) .== (toNullable $ pgInt4 cId)
  restrict -< (_node_typename n) .== (pgInt4 $ nodeTypeId NodeDocument)
  returnA  -< (_node_id n, _node_hyperdata n)

graphCorpusDocSearchQuery :: O.Query (NodeRead, NodeNodeReadNull)
graphCorpusDocSearchQuery = leftJoin queryNodeTable queryNodeNodeTable cond
  where
    cond :: (NodeRead, NodeNodeRead) -> Column PGBool
    cond (n, nn) = nodeNode_node1_id nn .== _node_id n
















-- | TODO [""] -> panic "error"
toTSQuery :: [Text] -> TSQuery
toTSQuery txt = UnsafeTSQuery txt


instance IsString TSQuery
  where
    fromString = UnsafeTSQuery . words . cs


instance ToField TSQuery
  where
    toField (UnsafeTSQuery xs)
      = Many  $ intersperse (Plain " && ")
              $ map (\q -> Many [ Plain "plainto_tsquery("
                                , Escape (cs q)
                                , Plain ")"
                                ]
                    ) xs

data Order    = Asc | Desc

instance ToField Order
  where
    toField Asc  = Plain "ASC"
    toField Desc = Plain "DESC"

-- TODO
-- FIX fav
-- ADD ngrams count
-- TESTS
textSearchQuery :: Query
textSearchQuery = "SELECT n.id, n.hyperdata->'publication_year'     \
\                   , n.hyperdata->'title'                          \
\                   , n.hyperdata->'source'                         \
\                   , n.hyperdata->'authors'                        \
\                   , COALESCE(nn.score,null)                       \
\                      FROM nodes n                                 \
\            LEFT JOIN nodes_nodes nn  ON nn.node2_id = n.id        \
\              WHERE                                                \
\                n.search @@ (?::tsquery)                           \
\                AND (n.parent_id = ? OR nn.node1_id = ?)           \
\                AND n.typename  = ?                                \
\                ORDER BY n.hyperdata -> 'publication_date' ?       \
\            offset ? limit ?;"

-- | Text Search Function for Master Corpus
-- TODO : text search for user corpus
-- Example:
-- textSearchTest :: ParentId -> TSQuery -> Cmd [(Int, Value, Value, Value, Value, Maybe Int)]
-- textSearchTest pId q = mkCmd $ \c -> textSearch c q pId 5 0 Asc
textSearch :: Connection 
           -> TSQuery -> ParentId
           -> Limit -> Offset -> Order
           -> IO [(Int,Value,Value,Value, Value, Maybe Int)]
textSearch conn q p l o ord = query conn textSearchQuery (q,p,p,typeId,ord,o,l)
  where
    typeId = nodeTypeId NodeDocument


