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

import Debug.Trace (trace)
import Control.Lens (_Just, (^.))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.Types (NgramsTerm(..))
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core
import Gargantext.Core.Text.Metrics.CharByChar (levenshtein)
import Gargantext.Core.Types (TableResult(..))
import Gargantext.Core.Types.Main
import Gargantext.Database
import Gargantext.Database.Action.Metrics.NgramsByContext (getContextsByNgramsOnlyUser)
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Hyperdata -- (HyperdataContact(..))
import Gargantext.Database.Admin.Types.Node -- (AnnuaireId, CorpusId, ListId, DocId, ContactId, NodeId)
import Gargantext.Database.Query.Prelude (leftJoin2, returnA, queryNodeNodeTable)
import Gargantext.Database.Query.Table.Node (defaultList)
import Gargantext.Database.Query.Table.Node.Children (getAllContacts)
import Gargantext.Database.Query.Table.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Query.Table.NodeContext_NodeContext (insertNodeContext_NodeContext)
import Gargantext.Database.Query.Table.NodeNode (insertNodeNode)
import Gargantext.Database.Schema.Ngrams -- (NgramsType(..))
import Gargantext.Database.Schema.Node
-- import Gargantext.Database.Schema.Context
import qualified Data.HashMap.Strict as HM
import Gargantext.Prelude hiding (sum)
import Opaleye
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Set            as Set
import qualified Data.Text           as Text

-- | isPairedWith
-- All NodeAnnuaire paired with a Corpus of NodeId nId:
-- isPairedWith NodeAnnuaire corpusId
isPairedWith :: NodeId -> NodeType -> Cmd err [NodeId]
isPairedWith nId nt = runOpaQuery (selectQuery nt nId)
  where
    selectQuery :: NodeType -> NodeId -> Select (Column SqlInt4)
    selectQuery nt' nId' = proc () -> do
      (node, node_node) <- queryJoin -< ()
      restrict -< (node^.node_typename)    .== (sqlInt4 $ toDBid nt')
      restrict -< (node_node^.nn_node1_id) .== (toNullable $ pgNodeId nId')
      returnA  -<  node^.node_id

    queryJoin :: Select (NodeRead, NodeNodeReadNull)
    queryJoin = leftJoin2 queryNodeTable queryNodeNodeTable cond
      where
        cond (node, node_node) = node^.node_id .== node_node^. nn_node2_id

-----------------------------------------------------------------------
pairing :: AnnuaireId -> CorpusId -> Maybe ListId -> GargNoServer [Int]
pairing a c l' = do
  l <- case l' of
    Nothing -> defaultList c
    Just l'' -> pure l''
  dataPaired <- dataPairing a (c,l,Authors)
  _ <- insertNodeNode [ NodeNode c a Nothing Nothing]
  insertNodeContext_NodeContext $ prepareInsert c a dataPaired


dataPairing :: AnnuaireId
             -> (CorpusId, ListId, NgramsType)
             -> GargNoServer (HashMap ContactId (Set DocId))
dataPairing aId (cId, lId, ngt) = do
  -- mc :: HM.HashMap ContactName (Set ContactId) 
  mc <- getNgramsContactId aId
  -- md :: HM.HashMap DocAuthor   (Set DocId)
  md <- getNgramsDocId     cId lId ngt
  -- printDebug "dataPairing authors" (HM.keys md)
  let result = fusion mc md
  -- printDebug "dataPairing" (length $ HM.keys result)
  pure result


prepareInsert :: CorpusId -> AnnuaireId -> HashMap ContactId (Set DocId)
              -> [(CorpusId, AnnuaireId, DocId, ContactId)]
prepareInsert corpusId annuaireId mapContactDocs =
  map (\(contactId,docId) -> (corpusId, docId, annuaireId, contactId))
        $ List.concat
        $ map (\(contactId, setDocIds)
                -> map (\setDocId
                         -> (contactId, setDocId)
                       ) $ Set.toList setDocIds
               )
        $ HM.toList mapContactDocs

------------------------------------------------------------------------
type ContactName = NgramsTerm
type DocAuthor   = NgramsTerm
type Projected   = NgramsTerm

fusion :: HashMap ContactName (Set ContactId)
       -> HashMap DocAuthor   (Set DocId)
       -> HashMap ContactId   (Set DocId)
fusion mc md = HM.fromListWith (<>)
             $ List.concat
             $ map (\(docAuthor, docs)
                     -> case (getClosest Text.toLower docAuthor (HM.keys mc)) of
                          Nothing -> []
                          Just author -> case HM.lookup author mc of
                                          Nothing    -> []
                                          Just contactIds -> map (\contactId -> (contactId, docs))
                                                                 $ Set.toList contactIds
                   )
             $ HM.toList md

fusion'' :: HashMap ContactName (Set ContactId)
       -> HashMap DocAuthor   (Set DocId)
       -> HashMap ContactId   (Set DocId)
fusion'' mc md = hashmapReverse $ fusion' mc (hashmapReverse md)


fusion' :: HashMap ContactName (Set ContactId)
       -> HashMap DocId (Set DocAuthor)
       -> HashMap DocId (Set ContactId)
fusion' mc md = HM.fromListWith (<>)
             $ map (\(docId, setAuthors) -> (docId, getContactIds mc $ getClosest' setAuthors (HM.keys mc)))
             $ HM.toList md

getContactIds :: HashMap ContactName (Set ContactId) -> Set ContactName -> Set ContactId
getContactIds mapContactNames contactNames =
  if Set.null contactNames
     then Set.empty
     else Set.unions $ catMaybes $ map (\contactName -> HM.lookup contactName mapContactNames) $ Set.toList contactNames

getClosest' :: Set DocAuthor -> [ContactName] -> Set ContactName
getClosest' setAuthors contactNames = trace (show (setAuthors, setContactNames)) $ setContactNames
  where
    setContactNames = if Set.null xs then ys else xs
    xs = Set.fromList $ catMaybes $ map (\author -> getClosest Text.toLower author contactNames) $ Set.toList setAuthors
    ys = Set.fromList $ catMaybes $ map (\(NgramsTerm author) -> case ((lastMay . (Text.splitOn " ")) author) of
                                                      Nothing      -> Nothing
                                                      Just authorReduced -> getClosest Text.toLower (NgramsTerm authorReduced) contactNames)
                                  $ Set.toList setAuthors


getClosest :: (Text -> Text) -> NgramsTerm -> [NgramsTerm] -> Maybe NgramsTerm
getClosest f (NgramsTerm from) candidates = fst <$> head scored
  where
    scored   = List.sortOn snd
             $ List.filter (\(_,score) -> score <= 2)
             $ map (\cand@(NgramsTerm candidate) -> (cand, levenshtein (f from) (f candidate))) candidates


------------------------------------------------------------------------
getNgramsContactId :: AnnuaireId
                   -> Cmd err (HashMap ContactName (Set NodeId))
getNgramsContactId aId = do
  contacts <- getAllContacts aId
  -- printDebug "getAllContexts" (tr_count contacts)
  let paired= HM.fromListWith (<>)
       $ map (\contact -> (toName contact, Set.singleton (contact^.node_id))
              ) (tr_docs contacts)
  -- printDebug "paired" (HM.keys paired)
  pure paired
-- POC here, should be a probabilistic function (see the one used to find lang)
toName :: Node HyperdataContact -> NgramsTerm
-- toName contact = NgramsTerm $ (Text.toTitle $ Text.take 1 firstName) <> ". " <> (Text.toTitle lastName)
toName contact = NgramsTerm $ (Text.toTitle firstName) <> " " <> (Text.toTitle lastName)
  where
    firstName = fromMaybe "" $ contact^.(node_hyperdata . hc_who . _Just . cw_firstName)
    lastName  = fromMaybe "" $ contact^.(node_hyperdata . hc_who . _Just . cw_lastName)

getNgramsDocId :: CorpusId
                -> ListId
                -> NgramsType
                -> GargNoServer (HashMap DocAuthor (Set NodeId))
getNgramsDocId cId lId nt = do
  lIds <- selectNodesWithUsername NodeList userMaster
  repo <- getRepo' (lId:lIds)
  let ngs = filterListWithRoot [MapTerm, CandidateTerm] $ mapTermListRoot (lId:lIds) nt repo
  -- printDebug "getNgramsDocId" ngs

  groupNodesByNgrams ngs <$> getContextsByNgramsOnlyUser cId (lIds <> [lId]) nt (HashMap.keys ngs)

hashmapReverse :: (Ord a, Eq b, Hashable b)
        => HashMap a (Set b) -> HashMap b (Set a)
hashmapReverse m = HM.fromListWith (<>)
          $ List.concat
          $ map (\(k,vs) -> [ (v, Set.singleton k) | v <- Set.toList vs])
          $ HM.toList m
