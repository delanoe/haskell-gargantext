{-|
Module      : Gargantext.Database.Metrics.NgramsByContext
Description : Ngrams by Node user and master
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams by node enable contextual metrics.

-}

{-# LANGUAGE QuasiQuotes       #-}

module Gargantext.Database.Action.Metrics.NgramsByContext
  where

--import Data.Map.Strict.Patch (PatchMap, Replace, diff)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra (first, second, swap)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
-- import Debug.Trace (trace)
import Gargantext.Core
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.Data.HashMap.Strict.Utils as HM
import Gargantext.Database.Admin.Types.Node (ListId, CorpusId, NodeId(..), ContextId, MasterCorpusId, NodeType(NodeDocument), UserCorpusId, DocId)
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Database.Schema.Ngrams (ngramsTypeId, NgramsType(..))
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Database.PostgreSQL.Simple as DPS

-- | fst is size of Supra Corpus
--   snd is Texts and size of Occurrences (different docs)

countContextsByNgramsWith :: (NgramsTerm -> NgramsTerm)
                       -> HashMap NgramsTerm (Set ContextId)
                       -> (Double, HashMap NgramsTerm (Double, Set NgramsTerm))
countContextsByNgramsWith f m = (total, m')
  where
    total = fromIntegral $ Set.size $ Set.unions $ HM.elems m
    m'    = HM.map ( swap . second (fromIntegral . Set.size))
          $ groupContextsByNgramsWith f m


    groupContextsByNgramsWith :: (NgramsTerm -> NgramsTerm)
                              -> HashMap NgramsTerm (Set NodeId)
                              -> HashMap NgramsTerm (Set NgramsTerm, Set ContextId)
    groupContextsByNgramsWith f' m'' =
      HM.fromListWith (<>) $ map (\(t,ns) -> (f' t, (Set.singleton t, ns)))
                           $ HM.toList m''

------------------------------------------------------------------------

getContextsByNgramsUser ::  HasDBid NodeType
                        => CorpusId
                        -> NgramsType
                        -> Cmd err (HashMap NgramsTerm (Set ContextId))
getContextsByNgramsUser cId nt =
  HM.fromListWith (<>) <$> map (\(n,t) -> (NgramsTerm t, Set.singleton n))
                    <$> selectNgramsByContextUser cId nt
    where

      selectNgramsByContextUser :: HasDBid NodeType
                                => CorpusId
                                -> NgramsType
                                -> Cmd err [(NodeId, Text)]
      selectNgramsByContextUser cId' nt' =
        runPGSQuery queryNgramsByContextUser
                    ( cId'
                    , toDBid NodeDocument
                    , ngramsTypeId nt'
           --         , 100 :: Int -- limit
           --         , 0   :: Int -- offset
                    )

      queryNgramsByContextUser :: DPS.Query
      queryNgramsByContextUser = [sql|
        SELECT cng.context_id, ng.terms FROM context_node_ngrams cng
          JOIN ngrams         ng ON cng.ngrams_id = ng.id
          JOIN nodes_contexts nc ON nc.context_id   = cng.context_id
          JOIN contexts        c ON nc.context_id   = c.id
          WHERE nc.node_id      = ? -- CorpusId
            AND c.typename      = ? -- toDBid
            AND cng.ngrams_type = ? -- NgramsTypeId
            AND nc.category     > 0 -- is not in Trash
            GROUP BY cng.context_id, ng.terms
        |]


------------------------------------------------------------------------
getOccByNgramsOnlyFast_withSample :: HasDBid NodeType
                       => CorpusId
                       -> Int
                       -> NgramsType
                       -> [NgramsTerm]
                       -> Cmd err (HashMap NgramsTerm Int)
getOccByNgramsOnlyFast_withSample cId int nt ngs =
  HM.fromListWith (+) <$> selectNgramsOccurrencesOnlyByContextUser_withSample cId int nt ngs


getOccByNgramsOnlyFast' :: CorpusId
                       -> ListId
                       -> NgramsType
                       -> [NgramsTerm]
                       -> Cmd err (HashMap NgramsTerm Int)
getOccByNgramsOnlyFast' cId lId nt tms = -- trace (show (cId, lId)) $
  HM.fromListWith (+) <$> map (second round) <$> run cId lId nt tms

    where
      fields = [QualifiedIdentifier Nothing "text"]

      run :: CorpusId
           -> ListId
           -> NgramsType
           -> [NgramsTerm]
           -> Cmd err [(NgramsTerm, Double)]
      run cId' lId' nt' tms' = map (first NgramsTerm) <$> runPGSQuery query
                ( Values fields ((DPS.Only . unNgramsTerm) <$> tms')
                , cId'
                , lId'
                , ngramsTypeId nt'
                )

      query :: DPS.Query
      query = [sql|
        WITH input_rows(terms) AS (?)
        SELECT ng.terms, nng.weight FROM nodes_contexts nc
          JOIN node_node_ngrams   nng  ON nng.node1_id  = nc.node_id
          JOIN ngrams              ng  ON nng.ngrams_id = ng.id
          JOIN input_rows          ir  ON ir.terms      = ng.terms
          WHERE nng.node1_id       = ?  -- CorpusId
            AND nng.node2_id       = ?  -- ListId
            AND nng.ngrams_type    = ?  -- NgramsTypeId
            AND nc.category        > 0  -- Not trash
            GROUP BY ng.terms, nng.weight
        |]


selectNgramsOccurrencesOnlyByContextUser_withSample :: HasDBid NodeType
                                      => CorpusId
                                      -> Int
                                      -> NgramsType
                                      -> [NgramsTerm]
                                      -> Cmd err [(NgramsTerm, Int)]
selectNgramsOccurrencesOnlyByContextUser_withSample cId int nt tms =
  fmap (first NgramsTerm) <$>
  runPGSQuery queryNgramsOccurrencesOnlyByContextUser_withSample
                ( int
                , toDBid NodeDocument
                , cId
                , Values fields ((DPS.Only . unNgramsTerm) <$> tms)
                , cId
                , ngramsTypeId nt
                )
    where
      fields = [QualifiedIdentifier Nothing "text"]

queryNgramsOccurrencesOnlyByContextUser_withSample :: DPS.Query
queryNgramsOccurrencesOnlyByContextUser_withSample = [sql|
  WITH nodes_sample AS (SELECT id FROM contexts n TABLESAMPLE SYSTEM_ROWS (?)
                          JOIN nodes_contexts nn ON n.id = nn.context_id
                            WHERE n.typename  = ?
                            AND nn.node_id = ?),
       input_rows(terms) AS (?)
  SELECT ng.terms, COUNT(cng.context_id) FROM context_node_ngrams cng
    JOIN ngrams ng      ON cng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN nodes_contexts nn ON nn.context_id   = cng.context_id
    JOIN nodes_sample n ON nn.context_id   = n.id
    WHERE nn.node_id      = ? -- CorpusId
      AND cng.ngrams_type = ? -- NgramsTypeId
      AND nn.category     > 0
      GROUP BY cng.node_id, ng.terms
  |]


------------------------------------------------------------------------

getContextsByNgramsOnlyUser :: HasDBid NodeType
                         => CorpusId
                         -> [ListId]
                         -> NgramsType
                         -> [NgramsTerm]
                         -> Cmd err (HashMap NgramsTerm (Set NodeId))
getContextsByNgramsOnlyUser cId ls nt ngs =
     HM.unionsWith        (<>)
   . map (HM.fromListWith (<>)
   . map (second Set.singleton))
  <$> mapM (selectNgramsOnlyByContextUser cId ls nt)
           (splitEvery 1000 ngs)

getNgramsByContextOnlyUser :: HasDBid NodeType
                        => NodeId
                        -> [ListId]
                        -> NgramsType
                        -> [NgramsTerm]
                        -> Cmd err (Map NodeId (Set NgramsTerm))
getNgramsByContextOnlyUser cId ls nt ngs =
     Map.unionsWith         (<>)
   . map ( Map.fromListWith (<>)
         . map (second Set.singleton)
         )
   . map (map swap)
  <$> mapM (selectNgramsOnlyByContextUser cId ls nt)
           (splitEvery 1000 ngs)

------------------------------------------------------------------------
-- used in G.Core.Text.List
selectNgramsOnlyByContextUser :: HasDBid NodeType
                           => CorpusId
                           -> [ListId]
                           -> NgramsType
                           -> [NgramsTerm]
                           -> Cmd err [(NgramsTerm, ContextId)]
selectNgramsOnlyByContextUser cId ls nt tms =
  fmap (first NgramsTerm) <$>
  runPGSQuery queryNgramsOnlyByContextUser
                ( Values fields ((DPS.Only . unNgramsTerm) <$> tms)
                , Values [QualifiedIdentifier Nothing "int4"]
                         (DPS.Only <$> (map (\(NodeId n) -> n) ls))
                , cId
                , toDBid NodeDocument
                , ngramsTypeId nt
                )
    where
      fields = [QualifiedIdentifier Nothing "text"]

queryNgramsOnlyByContextUser :: DPS.Query
queryNgramsOnlyByContextUser = [sql|
  WITH input_rows(terms) AS (?),
       input_list(id)    AS (?)
  SELECT ng.terms, cng.context_id FROM context_node_ngrams cng
    JOIN ngrams         ng ON cng.ngrams_id = ng.id
    JOIN input_rows     ir ON ir.terms      = ng.terms
    JOIN input_list     il ON il.id         = cng.node_id
    JOIN nodes_contexts nc ON nc.context_id   = cng.context_id
    JOIN contexts        c ON nc.context_id   = c.id
    WHERE nc.node_id      = ? -- CorpusId
      AND c.typename      = ? -- toDBid (maybe not useful with context table)
      AND cng.ngrams_type = ? -- NgramsTypeId
      AND nc.category     > 0
      GROUP BY ng.terms, cng.context_id
  |]

getNgramsByDocOnlyUser :: DocId
                       -> [ListId]
                       -> NgramsType
                       -> [NgramsTerm]
                       -> Cmd err (HashMap NgramsTerm (Set NodeId))
getNgramsByDocOnlyUser cId ls nt ngs =
  HM.unionsWith (<>)
  . map (HM.fromListWith (<>) . map (second Set.singleton))
  <$> mapM (selectNgramsOnlyByDocUser cId ls nt) (splitEvery 1000 ngs)


selectNgramsOnlyByDocUser :: DocId
                          -> [ListId]
                          -> NgramsType
                          -> [NgramsTerm]
                          -> Cmd err [(NgramsTerm, NodeId)]
selectNgramsOnlyByDocUser dId ls nt tms =
  fmap (first NgramsTerm) <$>
  runPGSQuery queryNgramsOnlyByDocUser
                ( Values fields ((DPS.Only . unNgramsTerm) <$> tms)
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
  SELECT ng.terms, cng.node_id FROM context_node_ngrams cng
    JOIN ngrams ng      ON cng.ngrams_id = ng.id
    JOIN input_rows  ir ON ir.terms      = ng.terms
    JOIN input_list  il ON il.id         = cng.context_id
    WHERE cng.node_id     = ? -- DocId
      AND cng.ngrams_type = ? -- NgramsTypeId
      GROUP BY ng.terms, cng.node_id
  |]

------------------------------------------------------------------------
-- | TODO filter by language, database, any social field
getContextsByNgramsMaster :: HasDBid NodeType
                          =>  UserCorpusId
                          -> MasterCorpusId
                          -> Cmd err (HashMap Text (Set NodeId))
getContextsByNgramsMaster ucId mcId = unionsWith (<>)
                                 . map (HM.fromListWith (<>) . map (\(n,t) -> (t, Set.singleton n)))
                                 -- . takeWhile (not . List.null)
                                 -- . takeWhile (\l -> List.length l > 3)
                                <$> mapM (selectNgramsByContextMaster 1000 ucId mcId) [0,500..10000]

selectNgramsByContextMaster :: HasDBid NodeType
                         => Int
                         -> UserCorpusId
                         -> MasterCorpusId
                         -> Int
                         -> Cmd err [(NodeId, Text)]
selectNgramsByContextMaster n ucId mcId p = runPGSQuery
                               queryNgramsByContextMaster'
                                 ( ucId
                                 , ngramsTypeId NgramsTerms
                                 , toDBid   NodeDocument
                                 , p
                                 , toDBid   NodeDocument
                                 , p
                                 , n
                                 , mcId
                                 , toDBid   NodeDocument
                                 , ngramsTypeId NgramsTerms
                                 )

-- | TODO fix context_node_ngrams relation
queryNgramsByContextMaster' :: DPS.Query
queryNgramsByContextMaster' = [sql|
  WITH contextsByNgramsUser AS (

  SELECT n.id, ng.terms FROM contexts n
    JOIN nodes_contexts  nn  ON n.id = nn.context_id
    JOIN context_node_ngrams cng ON cng.context_id   = n.id
    JOIN ngrams       ng  ON cng.ngrams_id = ng.id
    WHERE nn.node_id      = ?   -- UserCorpusId
      -- AND n.typename   = ?  -- toDBid
      AND cng.ngrams_type = ? -- NgramsTypeId
      AND nn.category > 0
      AND node_pos(n.id,?) >= ?
      AND node_pos(n.id,?) <  ?
    GROUP BY n.id, ng.terms

    ),

  contextsByNgramsMaster AS (

  SELECT n.id, ng.terms FROM contexts n TABLESAMPLE SYSTEM_ROWS(?)
    JOIN context_node_ngrams cng  ON n.id  = cng.context_id
    JOIN ngrams       ng   ON ng.id = cng.ngrams_id

    WHERE n.parent_id  = ?     -- Master Corpus toDBid
      AND n.typename   = ?     -- toDBid
      AND cng.ngrams_type = ? -- NgramsTypeId
    GROUP BY n.id, ng.terms
    )

  SELECT m.id, m.terms FROM nodesByNgramsMaster m
    RIGHT JOIN contextsByNgramsUser u ON u.id = m.id
  |]
