{-|
Module      : Gargantext.Database.Metrics.NgramsByNode
Description : Ngrams by Node user and master
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams by node enable special metrics.

-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.Metrics.NgramsByNode
  where

import Data.Map.Strict (Map, fromListWith, {-elems,-} toList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra (second)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Schema.Ngrams (ngramsTypeId, NgramsType(..))
import Gargantext.Database.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Utils (Cmd, runPGSQuery)
import Gargantext.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Database.PostgreSQL.Simple as DPS


type GlobalNodeId = NodeId
type LocalNodeId  = NodeId

joinNodesByNgrams :: Map Text (Set NodeId)
                  -> Map Text (Set NodeId)
                  -> Map Text (Set GlobalNodeId, Set LocalNodeId)
joinNodesByNgrams = undefined


countNodesByNgramsWith :: (Text -> Text) -> Map Text (Set NodeId)
                                         -> Map Text (Set Text, Int)
countNodesByNgramsWith f m = Map.map (second Set.size)
                           $ groupNodesByNgramsWith f m


groupNodesByNgramsWith :: (Text -> Text) -> Map Text (Set NodeId)
                                         -> Map Text (Set Text, Set NodeId)
groupNodesByNgramsWith f m =
      fromListWith (\a b -> (fst a <> fst b, snd a <> snd b))
    $ map (\(t,ns) -> (f t, (Set.singleton t, ns)))
    $ toList m

------------------------------------------------------------------------
getNodesByNgramsUser :: CorpusId -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsUser cId = fromListWith (<>) <$> map (\(n,t) -> (t, Set.singleton n))
                                         <$> selectNgramsByNodeUser cId

selectNgramsByNodeUser :: CorpusId -> Cmd err [(NodeId, Text)]
selectNgramsByNodeUser cId = runPGSQuery
                               queryNgramsByNodeUser
                                 ( cId
                                 , nodeTypeId NodeDocument
                                 , ngramsTypeId NgramsTerms
                                 )

queryNgramsByNodeUser :: DPS.Query
queryNgramsByNodeUser = [sql|

  SELECT nng.node_id, ng.terms FROM nodes_ngrams nng
    JOIN ngrams ng      ON nng.ngrams_id = ng.id
    JOIN nodes_nodes nn ON nn.node2_id   = nng.node_id
    JOIN nodes  n       ON nn.node2_id   = n.id
    WHERE nn.node1_id = ?     -- CorpusId
      AND n.typename  = ?     -- NodeTypeId
      AND nng.ngrams_type = ? -- NgramsTypeId
      AND nn.delete = False
      GROUP BY nng.node_id, ng.terms
  |]

------------------------------------------------------------------------

getNodesByNgramsMaster :: CorpusId -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsMaster cId = fromListWith (<>) <$> map (\(n,t) -> (t, Set.singleton n))
                                         <$> selectNgramsByNodeMaster cId

selectNgramsByNodeMaster :: CorpusId -> Cmd err [(NodeId, Text)]
selectNgramsByNodeMaster cId = runPGSQuery
                               queryNgramsByNodeMaster
                                 ( cId
                                 , nodeTypeId NodeDocument
                                 , ngramsTypeId NgramsTerms
                                 )

queryNgramsByNodeMaster :: DPS.Query
queryNgramsByNodeMaster = [sql|

  SELECT nng.node_id, ng.terms FROM nodes_ngrams nng
    JOIN ngrams ng ON ng.id = nng.ngrams_id
    JOIN nodes  n  ON n.id  = nng.node_id

    WHERE n.parent_id  = ?     -- Master Corpus NodeTypeId
      AND n.typename   = ?     -- NodeTypeId
      AND nng.ngrams_type  = ? -- NgramsTypeId
    
    GROUP BY nng.node_id, ng.terms
    LIMIT 10000 -- TODO remove the hard limit and limit with corpus only

  |]





