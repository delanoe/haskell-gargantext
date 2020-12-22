{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Arrows      #-}

module Gargantext.Database.Action.Flow.Pairing
  -- (pairing)
    where

import Control.Lens (_Just, (^.))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core
import Gargantext.Core.Types (TableResult(..))
import Gargantext.Core.Types.Main
import Gargantext.Database
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Hyperdata -- (HyperdataContact(..))
import Gargantext.Database.Admin.Types.Node -- (AnnuaireId, CorpusId, ListId, DocId, ContactId, NodeId)
import Gargantext.Database.Query.Prelude (leftJoin2, returnA, queryNodeNodeTable)
import Gargantext.Database.Query.Table.Node.Children (getAllContacts)
import Gargantext.Database.Query.Table.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.NodeNode (insertNodeNode)
import Gargantext.Database.Schema.Ngrams -- (NgramsType(..))
import Gargantext.Database.Schema.Node
import Gargantext.Prelude hiding (sum)
import Opaleye
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Set            as Set
import qualified Data.Text           as DT

-- | isPairedWith
-- All NodeAnnuaire paired with a Corpus of NodeId nId:
-- isPairedWith NodeAnnuaire corpusId
isPairedWith :: NodeId -> NodeType -> Cmd err [NodeId]
isPairedWith nId nt = runOpaQuery (selectQuery nt nId)
  where
    selectQuery :: NodeType -> NodeId -> Query (Column PGInt4)
    selectQuery nt' nId' = proc () -> do
      (node, node_node) <- queryJoin -< ()
      restrict -< (node^.node_typename)    .== (pgInt4 $ hasDBid nt')
      restrict -< (node_node^.nn_node1_id) .== (toNullable $ pgNodeId nId')
      returnA  -<  node^.node_id

    queryJoin :: Query (NodeRead, NodeNodeReadNull)
    queryJoin = leftJoin2 queryNodeTable queryNodeNodeTable cond
      where
        cond (node, node_node) = node^.node_id .== node_node^. nn_node2_id

-----------------------------------------------------------------------
pairing :: AnnuaireId -> CorpusId -> Maybe ListId -> GargNoServer Int
pairing a c l' = do
  l <- case l' of
    Nothing -> defaultList c
    Just l'' -> pure l''
  dataPaired <- dataPairing a (c,l,Authors) takeName takeName
  r <- insertDB $ prepareInsert dataPaired
  _ <- insertNodeNode [ NodeNode c a Nothing Nothing]
  pure r


dataPairing :: AnnuaireId
             -> (CorpusId, ListId, NgramsType)
             -> (ContactName -> Projected)
             -> (DocAuthor   -> Projected)
             -> GargNoServer (HashMap ContactId (Set DocId))
dataPairing aId (cId, lId, ngt) fc fa = do
  mc <- getNgramsContactId aId
  md <- getNgramsDocId cId lId ngt

  printDebug "ngramsContactId" mc
  printDebug "ngramsDocId"     md
  let
    from = projectionFrom (Set.fromList $ HM.keys mc) fc
    to   = projectionTo   (Set.fromList $ HM.keys md) fa

  pure $ fusion mc $ align from to md



prepareInsert :: HashMap ContactId (Set DocId) -> [NodeNode]
prepareInsert m =  map (\(n1,n2) -> NodeNode n1 n2 Nothing Nothing)
                $ List.concat
                $ map (\(contactId, setDocIds)
                        -> map (\setDocId
                                 -> (contactId, setDocId)
                               ) $ Set.toList setDocIds
                       )
                $ HM.toList m

------------------------------------------------------------------------
type ContactName = NgramsTerm
type DocAuthor   = NgramsTerm
type Projected   = NgramsTerm

projectionFrom :: Set ContactName -> (ContactName -> Projected) -> HashMap ContactName Projected
projectionFrom ss f = HM.fromList $ map (\s -> (s, f s)) (Set.toList ss)  -- use HS.toMap

projectionTo :: Set DocAuthor -> (DocAuthor -> Projected) -> HashMap Projected (Set DocAuthor)
projectionTo ss f = HM.fromListWith (<>) $ map (\s -> (f s, Set.singleton s)) (Set.toList ss)  -- use HS.toMap
------------------------------------------------------------------------
takeName :: NgramsTerm -> NgramsTerm
takeName (NgramsTerm texte) = NgramsTerm $ DT.toLower texte'
  where
    texte' = maybe texte (\x -> if DT.length x > 3 then x else texte)
                           (lastName' texte)
    lastName' = lastMay . DT.splitOn " "


------------------------------------------------------------------------
align :: HashMap ContactName Projected
      -> HashMap Projected (Set DocAuthor)
      -> HashMap DocAuthor (Set DocId)
      -> HashMap ContactName (Set DocId)
align mc ma md = HM.fromListWith (<>)
               $ map (\c -> (c, getProjection md $ testProjection c mc ma))
               $ HM.keys mc
  where
    getProjection :: HashMap DocAuthor (Set DocId) -> Set DocAuthor -> Set DocId
    getProjection ma' sa' =
      if Set.null sa'
         then Set.empty
         else Set.unions $ sets ma' sa'
           where
             sets ma'' sa'' = Set.map (\s -> lookup s ma'') sa''
             lookup s' ma''= fromMaybe Set.empty (HM.lookup s' ma'')

    testProjection :: ContactName
                   -> HashMap ContactName Projected
                   -> HashMap Projected (Set DocAuthor)
                   -> Set DocAuthor
    testProjection cn' mc' ma' = case HM.lookup cn' mc' of
      Nothing -> Set.empty
      Just  c -> case HM.lookup c ma' of
        Nothing -> Set.empty
        Just  a -> a

fusion :: HashMap ContactName (Set ContactId)
       -> HashMap ContactName (Set DocId)
       -> HashMap ContactId   (Set DocId)
fusion mc md = HM.fromListWith (<>)
             $ catMaybes
             $ [ (,) <$> Just cId <*> HM.lookup cn md
                      | (cn, setContactId) <- HM.toList mc
                      , cId <- Set.toList setContactId
               ]
------------------------------------------------------------------------

getNgramsContactId :: AnnuaireId
                   -> Cmd err (HashMap ContactName (Set NodeId))
getNgramsContactId aId = do
  contacts <- getAllContacts aId
  pure $ HM.fromListWith (<>)
       $ catMaybes
       $ map (\contact -> (,) <$> (NgramsTerm <$> contact^.(node_hyperdata . hc_who . _Just . cw_lastName))
                              <*> Just ( Set.singleton (contact^.node_id))
              ) (tr_docs contacts)


getNgramsDocId :: CorpusId
                -> ListId
                -> NgramsType
                -> GargNoServer (HashMap DocAuthor (Set NodeId))
getNgramsDocId cId lId nt = do
  repo <- getRepo
  lIds <- selectNodesWithUsername NodeList userMaster
  let ngs = filterListWithRoot MapTerm $ mapTermListRoot [lId] nt repo

  groupNodesByNgrams ngs
    <$> getNodesByNgramsOnlyUser cId (lIds <> [lId]) nt (HashMap.keys ngs)
