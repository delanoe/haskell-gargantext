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
import Data.Map (Map, fromList, fromListWith)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.Types (TableResult(..), Term)
import Gargantext.Core.Types.Main
import Gargantext.Database
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Admin.Types.Hyperdata -- (HyperdataContact(..))
import Gargantext.Database.Admin.Types.Node -- (AnnuaireId, CorpusId, ListId, DocId, ContactId, NodeId)
import Gargantext.Database.Prelude (Cmd, runOpaQuery)
import Gargantext.Database.Query.Prelude (leftJoin2, returnA, queryNodeNodeTable)
import Gargantext.Database.Query.Table.Node.Children (getAllContacts)
import Gargantext.Database.Query.Table.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Schema.Ngrams -- (NgramsType(..))
import Gargantext.Database.Schema.Node
import Gargantext.Prelude hiding (sum)
import Opaleye
import Safe (lastMay)
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as DT


-- | isPairedWith
-- All NodeAnnuaire paired with a Corpus of NodeId nId:
-- isPairedWith NodeAnnuaire corpusId
isPairedWith :: NodeType -> NodeId -> Cmd err [NodeId]
isPairedWith nt nId = runOpaQuery (selectQuery nt nId)
  where
    selectQuery :: NodeType -> NodeId -> Query (Column PGInt4)
    selectQuery nt' nId' = proc () -> do
      (node, node_node) <- queryJoin -< ()
      restrict -< (node^.node_typename)    .== (pgInt4 $ nodeTypeId nt')
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
  insertDB $ prepareInsert dataPaired


dataPairing :: AnnuaireId
             -> (CorpusId, ListId, NgramsType)
             -> (ContactName -> Projected)
             -> (DocAuthor   -> Projected)
             -> GargNoServer (Map ContactId (Set DocId))
dataPairing aId (cId, lId, ngt) fc fa = do
  mc <- getNgramsContactId aId
  md <- getNgramsDocId cId lId ngt

  printDebug "ngramsContactId" mc
  printDebug "ngramsDocId"     md
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
takeName :: Term -> Term
takeName texte = DT.toLower texte'
  where
    texte' = maybe texte (\x -> if DT.length x > 3 then x else texte)
                           (lastName' texte)
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


getNgramsDocId :: CorpusId
                -> ListId
                -> NgramsType
                -> GargNoServer (Map DocAuthor (Set NodeId))
getNgramsDocId cId lId nt = do
  repo <- getRepo
  lIds <- selectNodesWithUsername NodeList userMaster
  let ngs = filterListWithRoot MapTerm $ mapTermListRoot [lId] nt repo

  groupNodesByNgrams ngs
    <$> getNodesByNgramsOnlyUser cId (lIds <> [lId]) nt (Map.keys ngs)
