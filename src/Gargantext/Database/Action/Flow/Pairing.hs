{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes       #-}
-- {-# LANGUAGE Arrows #-}

module Gargantext.Database.Action.Flow.Pairing
  -- (pairing)
    where

import Data.Set (Set)
import Control.Lens (_Just, (^.))
import Data.Map (Map, fromList, fromListWith)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, toLower)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core.Types (TableResult(..))
import Gargantext.Database
import Gargantext.Database.Admin.Types.Hyperdata -- (HyperdataContact(..))
import Gargantext.Database.Admin.Types.Node -- (AnnuaireId, CorpusId, ListId, DocId, ContactId, NodeId)
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Database.Query.Table.Node.Children (getAllContacts)
import Gargantext.Database.Schema.Ngrams -- (NgramsType(..))
import Gargantext.Database.Schema.Node
import Gargantext.Prelude hiding (sum)
import Safe (lastMay)
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Text as DT
import qualified Data.Set  as Set

-- TODO mv this type in Types Main
type Terms = Text



{-
pairingPolicy :: (Terms -> Terms)
              -> NgramsT Ngrams
              -> NgramsT Ngrams
pairingPolicy f (NgramsT nt (Ngrams ng _)) = (NgramsT nt (Ngrams (f ng) 1))


pairMaps :: Map (NgramsT Ngrams) a
         -> Map (NgramsT Ngrams) NgramsId
         -> Map NgramsIndexed (Map NgramsType a)
pairMaps m1 m2 =
  DM.fromList
    [ (NgramsIndexed ng nId, DM.singleton nt n2i)
    | (k@(NgramsT nt ng),n2i) <- DM.toList m1
    , Just nId <- [DM.lookup k m2]
    ]
-}

-----------------------------------------------------------------------

pairing :: AnnuaireId -> CorpusId -> ListId -> Cmd err Int
pairing a c l = do
  dataPaired <- dataPairing a (c,l,Authors) lastName toLower
  insertDB $ prepareInsert dataPaired


dataPairing :: AnnuaireId
             -> (CorpusId, ListId, NgramsType)
             -> (ContactName -> Projected)
             -> (DocAuthor   -> Projected)
             -> Cmd err (Map ContactId (Set DocId))
dataPairing aId (cId, lId, ngt) fc fa = do
  mc <- getNgramsContactId aId
  md <- getNgramsDocId cId lId ngt

  let
    from = projectionFrom (Set.fromList $ Map.keys mc) fc
    to   = projectionTo   (Set.fromList $ Map.keys md) fa

  pure $ fusion mc $ align from to md



prepareInsert :: Map ContactId (Set DocId) -> [NodeNode]
prepareInsert m =  map (\(n1,n2) -> NodeNode n1 n2 Nothing Nothing)
                $ List.concat
                $ map (\(contactId, setDocIds)
                        -> map (\setDocId
                                 -> (contactId, setDocId)
                               ) $ Set.toList setDocIds
                       )
                $ Map.toList m



------------------------------------------------------------------------
type ContactName = Text
type DocAuthor   = Text
type Projected   = Text

projectionFrom :: Set ContactName -> (ContactName -> Projected) -> Map ContactName Projected
projectionFrom ss f = fromList $ map (\s -> (s, f s)) (Set.toList ss)

projectionTo :: Set DocAuthor -> (DocAuthor -> Projected) -> Map Projected (Set DocAuthor)
projectionTo ss f = fromListWith (<>) $ map (\s -> (f s, Set.singleton s)) (Set.toList ss)

------------------------------------------------------------------------
lastName :: Terms -> Terms
lastName texte = DT.toLower
               $ maybe texte (\x -> if DT.length x > 3 then x else texte)
                             (lastName' texte)
  where
    lastName' = lastMay . DT.splitOn " "


------------------------------------------------------------------------
align :: Map ContactName Projected
      -> Map Projected (Set DocAuthor)
      -> Map DocAuthor (Set DocId)
      -> Map ContactName (Set DocId)
align mc ma md = fromListWith (<>)
               $ map (\c -> (c, getProjection md $ testProjection c mc ma))
               $ Map.keys mc
  where
    getProjection :: Map DocAuthor (Set DocId) -> Set DocAuthor -> Set DocId
    getProjection ma' sa' =
      if Set.null sa'
         then Set.empty
         else Set.unions $ sets ma' sa'
           where
             sets ma'' sa'' = Set.map (\s -> lookup s ma'') sa''
             lookup s' ma''= fromMaybe Set.empty (Map.lookup s' ma'')

    testProjection :: ContactName
                   -> Map ContactName Projected
                   -> Map Projected (Set DocAuthor)
                   -> Set DocAuthor
    testProjection cn' mc' ma' = case Map.lookup cn' mc' of
      Nothing -> Set.empty
      Just  c -> case Map.lookup c ma' of
        Nothing -> Set.empty
        Just  a -> a

fusion :: Map ContactName (Set ContactId)
       -> Map ContactName (Set DocId)
       -> Map ContactId   (Set DocId)
fusion mc md = Map.fromListWith (<>)
             $ catMaybes
             $ [ (,) <$> Just cId <*> Map.lookup cn md
                      | (cn, setContactId) <- Map.toList mc
                      , cId <- Set.toList setContactId
               ]
------------------------------------------------------------------------

getNgramsContactId :: AnnuaireId
                   -> Cmd err (Map ContactName (Set NodeId))
getNgramsContactId aId = do
  contacts <- getAllContacts aId
  pure $ fromListWith (<>)
       $ catMaybes
       $ map (\contact -> (,) <$> contact^.(node_hyperdata . hc_who . _Just . cw_lastName)
                              <*> Just ( Set.singleton (contact^.node_id))
              ) (tr_docs contacts)

-- | TODO
-- filter Trash / map Authors
-- Indexing all ngramsType like Authors
getNgramsDocId :: CorpusId
                  -> ListId
                  -> NgramsType
                  -> Cmd err (Map DocAuthor (Set NodeId))
getNgramsDocId corpusId listId nt
  = fromListWith (<>)
  <$> map (\(t,nId) -> (t, Set.singleton (NodeId nId)))
  <$> selectNgramsDocId corpusId listId nt


selectNgramsDocId :: CorpusId
                   -> ListId
                   -> NgramsType
                   -> Cmd err [(Text, Int)]
selectNgramsDocId corpusId' listId' ngramsType' =
 runPGSQuery selectQuery (corpusId', listId', ngramsTypeId ngramsType')
   where
     selectQuery = [sql| SELECT ng.terms,nnng.node2_id from ngrams ng
                    JOIN node_node_ngrams nnng ON nnng.ngrams_id = ng.id
                    JOIN nodes_nodes      nn   ON nn.node2_id    = nnng.node2_id

                    WHERE nn.node1_id = ?
                      AND nnng.node1_id    = ?
                      AND nnng.ngrams_type = ?
                    ;
                   |]

