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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Action.Metrics.NgramsByNode
  where

import Data.Map.Strict (Map, fromListWith, elems, toList, fromList)
import Data.Map.Strict.Patch (PatchMap, Replace, diff)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra (second, swap)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Debug.Trace (trace)
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Admin.Utils (Cmd, runPGSQuery)
import Gargantext.Database.Schema.Ngrams (ngramsTypeId, NgramsType(..))
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
ngramsGroup :: Lang
            -> Int
            -> Int
            -> Text
            -> Text
ngramsGroup l _m _n = Text.intercalate " "
                  . map (stem l)
                  -- . take n
                  . List.sort
                  -- . (List.filter (\t -> Text.length t > m))
                  . Text.splitOn " "
                  . Text.replace "-" " "


sortTficf :: (Map Text (Double, Set Text))
          -> [   (Text,(Double, Set Text))]
sortTficf  = List.sortOn (fst . snd) . toList


getTficf :: UserCorpusId
         -> MasterCorpusId
         -> NgramsType
         -> (Text -> Text)
         -> Cmd err (Map Text (Double, Set Text))
getTficf u m nt f = do
  u' <- getNodesByNgramsUser   u nt
  m' <- getNodesByNgramsMaster u m

  pure $ toTficfData (countNodesByNgramsWith f u')
                     (countNodesByNgramsWith f m')

{-
getTficfWith :: UserCorpusId
             -> MasterCorpusId
             -> [ListId]
             -> NgramsType
             -> Map Text (Maybe Text)
             -> Cmd err (Map Text (Double, Set Text))
getTficfWith u m ls nt mtxt = do
  u' <- getNodesByNgramsOnlyUser   u ls nt (Map.keys mtxt)
  m' <- getNodesByNgramsMaster     u m

  let f x = case Map.lookup x mtxt of
        Nothing -> x
        Just x' -> maybe x identity x'

  pure $ toTficfData (countNodesByNgramsWith f u')
                     (countNodesByNgramsWith f m')
-}

type Context = (Double, Map Text (Double, Set Text))
type Supra   = Context
type Infra   = Context

toTficfData :: Infra
            -> Supra
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
      fromListWith (<>) $ map (\(t,ns) -> (f t, (Set.singleton t, ns)))
                        $ toList m

------------------------------------------------------------------------
getNodesByNgramsUser :: CorpusId
                     -> NgramsType
                     -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsUser cId nt =
  fromListWith (<>) <$> map (\(n,t) -> (t, Set.singleton n))
                    <$> selectNgramsByNodeUser cId nt
    where

      selectNgramsByNodeUser :: CorpusId
                             -> NgramsType
                             -> Cmd err [(NodeId, Text)]
      selectNgramsByNodeUser cId' nt' =
        runPGSQuery queryNgramsByNodeUser
                    ( cId'
                    , nodeTypeId NodeDocument
                    , ngramsTypeId nt'
           --         , 100 :: Int -- limit
           --         , 0   :: Int -- offset
                    )

      queryNgramsByNodeUser :: DPS.Query
      queryNgramsByNodeUser = [sql|
        SELECT nng.node2_id, ng.terms FROM node_node_ngrams nng
          JOIN ngrams ng      ON nng.ngrams_id = ng.id
          JOIN nodes_nodes nn ON nn.node2_id   = nng.node2_id
          JOIN nodes  n       ON nn.node2_id   = n.id
          WHERE nn.node1_id = ?     -- CorpusId
            AND n.typename  = ?     -- NodeTypeId
            AND nng.ngrams_type = ? -- NgramsTypeId
            AND nn.category > 0
            GROUP BY nng.node2_id, ng.terms
            ORDER BY (nng.node2_id, ng.terms) DESC
          --   LIMIT ?
          --  OFFSET ?
        |]
------------------------------------------------------------------------
-- TODO add groups
getOccByNgramsOnlyFast :: CorpusId
                       -> NgramsType
                       -> [Text]
                       -> Cmd err (Map Text Int)
getOccByNgramsOnlyFast cId nt ngs =
  fromListWith (+) <$> selectNgramsOccurrencesOnlyByNodeUser cId nt ngs


getOccByNgramsOnlyFast' :: CorpusId
                       -> ListId
                       -> NgramsType
                       -> [Text]
                       -> Cmd err (Map Text Int)
getOccByNgramsOnlyFast' cId lId nt tms = trace (show (cId, lId)) $ 
  fromListWith (+) <$> map (second round) <$> run cId lId nt tms

    where
      fields = [QualifiedIdentifier Nothing "text"]

      run :: CorpusId
           -> ListId
           -> NgramsType
           -> [Text]
           -> Cmd err [(Text, Double)]
      run cId' lId' nt' tms' = runPGSQuery query
                ( Values fields (DPS.Only <$> tms')
                , cId'
                , lId'
                , ngramsTypeId nt'
                )

      query :: DPS.Query
      query = [sql|
        WITH input_rows(terms) AS (?)
        SELECT ng.terms, nng.weight FROM node_node_ngrams nng
          JOIN ngrams ng      ON nng.ngrams_id = ng.id
          JOIN input_rows  ir ON ir.terms      = ng.terms
          WHERE nng.node1_id     = ?   -- CorpusId
            AND nng.node2_id     = ?   -- ListId
            AND nng.ngrams_type  = ? -- NgramsTypeId
            -- AND nn.category     > 0 -- TODO
            GROUP BY ng.terms, nng.weight
        |]


-- just slower than getOccByNgramsOnlyFast
getOccByNgramsOnlySlow :: NodeType
                       -> CorpusId
                       -> [ListId]
                       -> NgramsType
                       -> [Text]
                       -> Cmd err (Map Text Int)
getOccByNgramsOnlySlow t cId ls nt ngs =
  Map.map Set.size <$> getScore' t cId ls nt ngs
    where
      getScore' NodeCorpus   = getNodesByNgramsOnlyUser
      getScore' NodeDocument = getNgramsByDocOnlyUser
      getScore' _            = getNodesByNgramsOnlyUser

getOccByNgramsOnlySafe :: CorpusId
                       -> [ListId]
                       -> NgramsType
                       -> [Text]
                       -> Cmd err (Map Text Int)
getOccByNgramsOnlySafe cId ls nt ngs = do
  printDebug "getOccByNgramsOnlySafe" (cId, nt, length ngs)
  fast <- getOccByNgramsOnlyFast cId nt ngs
  slow <- getOccByNgramsOnlySlow NodeCorpus cId ls nt ngs
  when (fast /= slow) $
    printDebug "getOccByNgramsOnlySafe: difference"
               (diff slow fast :: PatchMap Text (Replace (Maybe Int)))
  pure slow


selectNgramsOccurrencesOnlyByNodeUser :: CorpusId
                                      -> NgramsType
                                      -> [Text]
                                      -> Cmd err [(Text, Int)]
selectNgramsOccurrencesOnlyByNodeUser cId nt tms =
  runPGSQuery queryNgramsOccurrencesOnlyByNodeUser
                ( Values fields (DPS.Only <$> tms)
                , cId
                , nodeTypeId NodeDocument
                , ngramsTypeId nt
                )
    where
      fields = [QualifiedIdentifier Nothing "text"]

-- same as queryNgramsOnlyByNodeUser but using COUNT on the node ids.
-- Question: with the grouping is the result exactly the same (since Set NodeId for 
-- equivalent ngrams intersections are not empty)
queryNgramsOccurrencesOnlyByNodeUser :: DPS.Query
queryNgramsOccurrencesOnlyByNodeUser = [sql|
  WITH input_rows(terms) AS (?)
  SELECT ng.terms, COUNT(nng.node2_id) FROM node_node_ngrams nng
    JOIN ngrams ng      ON nng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN nodes_nodes nn ON nn.node2_id   = nng.node2_id
    JOIN nodes  n       ON nn.node2_id   = n.id
    WHERE nn.node1_id     = ? -- CorpusId
      AND n.typename      = ? -- NodeTypeId
      AND nng.ngrams_type = ? -- NgramsTypeId
      AND nn.category     > 0
      GROUP BY nng.node2_id, ng.terms
  |]

queryNgramsOccurrencesOnlyByNodeUser' :: DPS.Query
queryNgramsOccurrencesOnlyByNodeUser' = [sql|
  WITH input_rows(terms) AS (?)
  SELECT ng.terms, COUNT(nng.node2_id) FROM node_node_ngrams nng
    JOIN ngrams ng      ON nng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN nodes_nodes nn ON nn.node2_id   = nng.node2_id
    JOIN nodes  n       ON nn.node2_id   = n.id
    WHERE nn.node1_id     = ? -- CorpusId
      AND n.typename      = ? -- NodeTypeId
      AND nng.ngrams_type = ? -- NgramsTypeId
      AND nn.category     > 0
      GROUP BY nng.node2_id, ng.terms
  |]

------------------------------------------------------------------------
getNodesByNgramsOnlyUser :: NodeId
                         -> [ListId]
                         -> NgramsType
                         -> [Text]
                         -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsOnlyUser cId ls nt ngs =
     Map.unionsWith    (<>)
   . map (fromListWith (<>)
   . map (second Set.singleton))
  <$> mapM (selectNgramsOnlyByNodeUser cId ls nt)
           (splitEvery 1000 ngs)


getNgramsByNodeOnlyUser :: NodeId
                        -> [ListId]
                        -> NgramsType
                        -> [Text]
                        -> Cmd err (Map NodeId (Set Text))
getNgramsByNodeOnlyUser cId ls nt ngs =
     Map.unionsWith    (<>)
   . map (fromListWith (<>)
   . map (second Set.singleton))
   . map (map swap)
  <$> mapM (selectNgramsOnlyByNodeUser cId ls nt)
           (splitEvery 1000 ngs)

------------------------------------------------------------------------
selectNgramsOnlyByNodeUser :: CorpusId
                           -> [ListId]
                           -> NgramsType
                           -> [Text]
                           -> Cmd err [(Text, NodeId)]
selectNgramsOnlyByNodeUser cId ls nt tms =
  runPGSQuery queryNgramsOnlyByNodeUser
                ( Values fields (DPS.Only <$> tms)
                , Values [QualifiedIdentifier Nothing "int4"] 
                         (DPS.Only <$> (map (\(NodeId n) -> n) ls))
                , cId
                , nodeTypeId NodeDocument
                , ngramsTypeId nt
                )
    where
      fields = [QualifiedIdentifier Nothing "text"]

queryNgramsOnlyByNodeUser :: DPS.Query
queryNgramsOnlyByNodeUser = [sql|
  WITH input_rows(terms) AS (?),
       input_list(id)    AS (?)
  SELECT ng.terms, nng.node2_id FROM node_node_ngrams nng
    JOIN ngrams ng      ON nng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN input_list  il ON il.id         = nng.node1_id
    JOIN nodes_nodes nn ON nn.node2_id   = nng.node2_id
    JOIN nodes  n       ON nn.node2_id   = n.id
    WHERE nn.node1_id     = ? -- CorpusId
      AND n.typename      = ? -- NodeTypeId
      AND nng.ngrams_type = ? -- NgramsTypeId
      AND nn.category     > 0
      GROUP BY ng.terms, nng.node2_id
  |]


selectNgramsOnlyByNodeUser' :: CorpusId
                            -> [ListId]
                            -> NgramsType
                            -> [Text]
                            -> Cmd err [(Text, Int)]
selectNgramsOnlyByNodeUser' cId ls nt tms =
  runPGSQuery queryNgramsOnlyByNodeUser
                ( Values fields (DPS.Only <$> tms)
                , Values [QualifiedIdentifier Nothing "int4"]
                         (DPS.Only <$> (map (\(NodeId n) -> n) ls))
                , cId
                , nodeTypeId NodeDocument
                , ngramsTypeId nt
                )
    where
      fields = [QualifiedIdentifier Nothing "text"]

queryNgramsOnlyByNodeUser' :: DPS.Query
queryNgramsOnlyByNodeUser' = [sql|
  WITH input_rows(terms) AS (?),
       input_list(id)    AS (?)
  SELECT ng.terms, nng.weight FROM node_node_ngrams nng
    JOIN ngrams ng      ON nng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN input_list  il ON il.id         = nng.node2_id
    WHERE nng.node1_id     = ? -- CorpusId
      AND nng.ngrams_type = ? -- NgramsTypeId
      -- AND nn.category     > 0
      GROUP BY ng.terms, nng.weight
  |]


getNgramsByDocOnlyUser :: NodeId
                       -> [ListId]
                       -> NgramsType
                       -> [Text]
                       -> Cmd err (Map Text (Set NodeId))
getNgramsByDocOnlyUser cId ls nt ngs =
  Map.unionsWith (<>)
  . map (fromListWith (<>) . map (second Set.singleton))
  <$> mapM (selectNgramsOnlyByDocUser cId ls nt) (splitEvery 1000 ngs)


selectNgramsOnlyByDocUser :: DocId
                          -> [ListId]
                          -> NgramsType
                          -> [Text]
                          -> Cmd err [(Text, NodeId)]
selectNgramsOnlyByDocUser dId ls nt tms =
  runPGSQuery queryNgramsOnlyByDocUser
                ( Values fields (DPS.Only <$> tms)
                , Values [QualifiedIdentifier Nothing "int4"]
                         (DPS.Only <$> (map (\(NodeId n) -> n) ls))
                , dId
                , ngramsTypeId nt
                )
    where
      fields = [QualifiedIdentifier Nothing "text"]


queryNgramsOnlyByDocUser :: DPS.Query
queryNgramsOnlyByDocUser = [sql|
  WITH input_rows(terms) AS (?),
       input_list(id)    AS (?)
  SELECT ng.terms, nng.node2_id FROM node_node_ngrams nng
    JOIN ngrams ng      ON nng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN input_list  il ON il.id         = nng.node1_id
    WHERE nng.node2_id     = ? -- DocId
      AND nng.ngrams_type = ? -- NgramsTypeId
      GROUP BY ng.terms, nng.node2_id
  |]

------------------------------------------------------------------------
-- | TODO filter by language, database, any social field
getNodesByNgramsMaster :: UserCorpusId -> MasterCorpusId -> Cmd err (Map Text (Set NodeId))
getNodesByNgramsMaster ucId mcId = Map.unionsWith (<>)
                                 . map (fromListWith (<>) . map (\(n,t) -> (t, Set.singleton n)))
                                 -- . takeWhile (not . List.null)
                                 -- . takeWhile (\l -> List.length l > 3)
                                <$> mapM (selectNgramsByNodeMaster 1000 ucId mcId) [0,500..10000]

selectNgramsByNodeMaster :: Int
                         -> UserCorpusId
                         -> MasterCorpusId
                         -> Int
                         -> Cmd err [(NodeId, Text)]
selectNgramsByNodeMaster n ucId mcId p = runPGSQuery
                               queryNgramsByNodeMaster'
                                 ( ucId
                                 , ngramsTypeId NgramsTerms
                                 , nodeTypeId   NodeDocument
                                 , p
                                 , nodeTypeId   NodeDocument
                                 , p
                                 , n
                                 , mcId
                                 , nodeTypeId   NodeDocument
                                 , ngramsTypeId NgramsTerms
                                 )

-- | TODO fix node_node_ngrams relation
queryNgramsByNodeMaster' :: DPS.Query
queryNgramsByNodeMaster' = [sql|
  WITH nodesByNgramsUser AS (

  SELECT n.id, ng.terms FROM nodes n
    JOIN nodes_nodes  nn  ON n.id = nn.node2_id
    JOIN node_node_ngrams nng ON nng.node2_id   = n.id
    JOIN ngrams       ng  ON nng.ngrams_id = ng.id
    WHERE nn.node1_id     = ?   -- UserCorpusId
      -- AND n.typename   = ?  -- NodeTypeId
      AND nng.ngrams_type = ? -- NgramsTypeId
      AND nn.category > 0
      AND node_pos(n.id,?) >= ?
      AND node_pos(n.id,?) <  ?
    GROUP BY n.id, ng.terms

    ),

  nodesByNgramsMaster AS (

  SELECT n.id, ng.terms FROM nodes n TABLESAMPLE SYSTEM_ROWS(?)
    JOIN node_node_ngrams nng  ON n.id  = nng.node2_id
    JOIN ngrams       ng   ON ng.id = nng.ngrams_id

    WHERE n.parent_id  = ?     -- Master Corpus NodeTypeId
      AND n.typename   = ?     -- NodeTypeId
      AND nng.ngrams_type = ? -- NgramsTypeId
    GROUP BY n.id, ng.terms
    )

  SELECT m.id, m.terms FROM nodesByNgramsMaster m
    RIGHT JOIN nodesByNgramsUser u ON u.id = m.id
  |]
