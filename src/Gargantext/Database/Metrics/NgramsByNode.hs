{-|
Module      : Gargantext.Database.Metrics.NgramsByNode
Description : Ngrams by Node user and master
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams by node enable contextual metrics.

-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Metrics.NgramsByNode
  where

import Data.Map.Strict (Map, fromListWith, elems, toList, fromList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra (second, swap)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Core (Lang(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Schema.Ngrams (ngramsTypeId, NgramsType(..))
import Gargantext.Database.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Utils (Cmd, runPGSQuery)
import Gargantext.Prelude
import Gargantext.Text.Metrics.TFICF
import Gargantext.Text.Terms.Mono.Stem (stem)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as DPS

-- | TODO: group with 2 terms only can be
-- discussed. Main purpose of this is offering
-- a first grouping option to user and get some
-- enriched data to better learn and improve that algo
ngramsGroup :: Lang -> Int -> Int -> Text -> Text
ngramsGroup l m n = Text.intercalate " "
                . map (stem l)
                . take n
                . List.sort
                . (List.filter (\t -> Text.length t > m))
                . Text.splitOn " "
                . Text.replace "-" " "


sortTficf :: (Map Text (Double, Set Text))
          -> [   (Text,(Double, Set Text))]
sortTficf  = List.sortOn (fst . snd) . toList


getTficf' :: UserCorpusId -> MasterCorpusId -> (Text -> Text)
         -> Cmd err (Map Text (Double, Set Text))
getTficf' u m f = do
  u' <- getNodesByNgramsUser   u NgramsTerms
  m' <- getNodesByNgramsMaster u m

  pure $ toTficfData (countNodesByNgramsWith f u')
                     (countNodesByNgramsWith f m')


type Context = (Double, Map Text (Double, Set Text))
type Supra   = Context
type Infra   = Context

toTficfData :: Infra -> Supra
            -> Map Text (Double, Set Text)
toTficfData (ti, mi) (ts, ms) =
  fromList [ (t, ( tficf (TficfInfra (Count n                              )(Total ti))
                         (TficfSupra (Count $ maybe 0 fst $ Map.lookup t ms)(Total ts))
                 , ns
                 )
             )
           | (t, (n,ns)) <- toList mi
           ]


-- | fst is size of Supra Corpus
--   snd is Texts and size of Occurrences (different docs)
countNodesByNgramsWith :: (Text -> Text)
                       -> Map Text (Set NodeId)
                       -> (Double, Map Text (Double, Set Text))
countNodesByNgramsWith f m = (total, m')
  where
    total = fromIntegral $ Set.size $ Set.unions $ elems m
    m'    = Map.map ( swap . second (fromIntegral . Set.size))
                    $ groupNodesByNgramsWith f m


groupNodesByNgramsWith :: (Text -> Text)
                       -> Map Text (Set NodeId)
                       -> Map Text (Set Text, Set NodeId)
groupNodesByNgramsWith f m =
      fromListWith (\a b -> (fst a <> fst b, snd a <> snd b))
    $ map (\(t,ns) -> (f t, (Set.singleton t, ns)))
    $ toList m

------------------------------------------------------------------------
getNodesByNgramsUser :: CorpusId -> NgramsType
                     -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsUser cId nt =
  fromListWith (<>) <$> map (\(n,t) -> (t, Set.singleton n))
                    <$> selectNgramsByNodeUser cId nt

selectNgramsByNodeUser :: CorpusId -> NgramsType
                       -> Cmd err [(NodeId, Text)]
selectNgramsByNodeUser cId nt =
  runPGSQuery queryNgramsByNodeUser
              ( cId
              , nodeTypeId NodeDocument
              , ngramsTypeId nt
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
-- TODO add groups
getOccByNgramsOnly :: CorpusId -> NgramsType -> [Text]
                   -> Cmd err (Map Text Int)
getOccByNgramsOnly cId nt ngs = Map.map Set.size
                             <$> getNodesByNgramsOnlyUser cId nt ngs

getNodesByNgramsOnlyUser :: CorpusId -> NgramsType -> [Text]
                         -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsOnlyUser cId nt ngs =
  fromListWith (<>) <$> map (\(n,t) -> (t, Set.singleton n))
                    <$> selectNgramsOnlyByNodeUser cId nt ngs


selectNgramsOnlyByNodeUser :: CorpusId -> NgramsType -> [Text]
                           -> Cmd err [(NodeId, Text)]
selectNgramsOnlyByNodeUser cId nt tms =
  runPGSQuery queryNgramsOnlyByNodeUser (DPS.Only $ Values fields tms' )
    where
      fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4", "int4", "int4"]
      tms'   = map (\t -> (t,cId,nodeTypeId NodeDocument, ngramsTypeId nt)) tms

queryNgramsOnlyByNodeUser :: DPS.Query
queryNgramsOnlyByNodeUser = [sql|

  WITH input_rows(terms,corpus_id,docType,ngramsType) AS (?)
  SELECT nng.node_id, ng.terms FROM nodes_ngrams nng
    JOIN ngrams ng      ON nng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN nodes_nodes nn ON nn.node2_id   = nng.node_id
    JOIN nodes  n       ON nn.node2_id   = n.id
    WHERE nn.node1_id = ir.corpus_id      -- CorpusId
      AND n.typename  = ir.docType        -- NodeTypeId
      AND nng.ngrams_type = ir.ngramsType -- NgramsTypeId
      AND nn.delete = False
      GROUP BY nng.node_id, ng.terms
  |]



------------------------------------------------------------------------
-- | TODO filter by language, database, any social field
getNodesByNgramsMaster :: UserCorpusId -> MasterCorpusId -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsMaster ucId mcId = fromListWith (<>) <$> map (\(n,t) -> (t, Set.singleton n))
                                         <$> selectNgramsByNodeMaster ucId mcId

selectNgramsByNodeMaster :: UserCorpusId -> MasterCorpusId -> Cmd err [(NodeId, Text)]
selectNgramsByNodeMaster ucId mcId = runPGSQuery
                               queryNgramsByNodeMaster
                                 ( ucId
                                 , nodeTypeId NodeDocument
                                 , ngramsTypeId NgramsTerms
                                 , mcId
                                 , nodeTypeId NodeDocument
                                 , ngramsTypeId NgramsTerms
                                 )

queryNgramsByNodeMaster :: DPS.Query
queryNgramsByNodeMaster = [sql|

WITH nodesByNgramsUser AS (

SELECT nng.node_id, ng.id, ng.terms FROM nodes_ngrams nng
  JOIN ngrams ng      ON nng.ngrams_id = ng.id
  JOIN nodes_nodes nn ON nn.node2_id   = nng.node_id
  JOIN nodes  n       ON nn.node2_id   = n.id
  WHERE nn.node1_id = ?   -- UserCorpusId
    AND n.typename  = ?     -- NodeTypeId
    AND nng.ngrams_type = ? -- NgramsTypeId
    AND nn.delete = False
    GROUP BY nng.node_id, ng.id, ng.terms
  ),

nodesByNgramsMaster AS (

SELECT nng.node_id, ng.id, ng.terms FROM nodes_ngrams nng
  JOIN ngrams ng ON ng.id = nng.ngrams_id
  JOIN nodes  n  ON n.id  = nng.node_id

  WHERE n.parent_id  = ?     -- Master Corpus NodeTypeId
    AND n.typename   = ?     -- NodeTypeId
    AND nng.ngrams_type = ? -- NgramsTypeId
  GROUP BY nng.node_id, ng.id, ng.terms)

SELECT m.node_id, m.terms FROM nodesByNgramsMaster m
RIGHT JOIN nodesByNgramsUser u ON u.id = m.id

  |]




