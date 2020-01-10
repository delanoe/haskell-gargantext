{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-- TODO-ACCESS:
--   check userId       CanFillUserCorpus   userCorpusId
--   check masterUserId CanFillMasterCorpus masterCorpusId

-- TODO-ACCESS: check uId CanInsertDoc pId && checkDocType nodeType
-- TODO-EVENTS: InsertedNodes
-}

{-# OPTIONS_GHC -fno-warn-orphans    #-}

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}

module Gargantext.Database.Flow -- (flowDatabase, ngrams2list)
  ( FlowCmdM
  , flowCorpusFile
  , flowCorpus
  , flowCorpusSearchInDatabase
  , getOrMkRoot
  , getOrMk_RootWithCorpus
  , flowAnnuaire
  )
    where

import Prelude (String)
import Data.Either
import Data.Tuple.Extra (first, second)
import Data.Traversable (traverse)
import Debug.Trace (trace)
import Control.Lens ((^.), view, _Just)
import Control.Monad.IO.Class (liftIO)
import Data.List (concat)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Monoid
import Data.Text (Text, splitOn, intercalate)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (NodePoly(..), Terms(..))
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Core.Flow.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Config (userMaster, corpusMasterName)
import Gargantext.Database.Flow.Utils (insertDocNgrams)
import Gargantext.Database.Flow.List
import Gargantext.Database.Flow.Types
import Gargantext.Database.Node.Contact -- (HyperdataContact(..), ContactWho(..))
import Gargantext.Database.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Root (getRoot)

import Gargantext.Database.Schema.Ngrams -- (insertNgrams, Ngrams(..), NgramsIndexed(..), indexNgrams,  NgramsType(..), text2ngrams, ngramsTypeId)
import Gargantext.Database.Schema.Node -- (mkRoot, mkCorpus, getOrMkList, mkGraph, {-mkPhylo,-} mkDashboard, mkAnnuaire, getCorporaWithParentId, HasNodeError, NodeError(..), nodeError)
import Gargantext.Database.Schema.NodeNgrams (listInsertDb, getCgramsId)
import Gargantext.Database.Schema.NodeNodeNgrams2 -- (NodeNodeNgrams2, insertNodeNodeNgrams2)
import Gargantext.Database.Schema.User (getUser, UserLight(..))
import Gargantext.Database.TextSearch (searchInDatabase)
import Gargantext.Database.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Utils (Cmd)
import Gargantext.Ext.IMT (toSchoolName)
import Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
import Gargantext.Prelude
import Gargantext.Text.Terms.Eleve (buildTries, toToken)
--import Gargantext.Text.List (buildNgramsLists,StopSize(..))
import Gargantext.Text.Corpus.Parsers (parseFile, FileFormat)
import qualified Gargantext.Text.Corpus.API.Isidore as Isidore
import Gargantext.Text.Terms (TermType(..), tt_lang, extractTerms, uniText)
import Gargantext.Text.Terms.Mono.Stem.En (stemIt)
import Gargantext.Prelude.Utils hiding (sha)
import System.FilePath (FilePath)
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified Gargantext.Database.Node.Document.Add  as Doc  (add)
import qualified Gargantext.Text.Corpus.Parsers.GrandDebat as GD

------------------------------------------------------------------------

data ApiQuery = ApiIsidoreQuery Text | ApiIsidoreAuth Text
-- | APIs
-- TODO instances
getDataApi :: Lang
           -> Maybe Limit
           -> ApiQuery
           -> IO [HyperdataDocument]
getDataApi lang limit (ApiIsidoreQuery q) = Isidore.get lang limit (Just q) Nothing
getDataApi lang limit (ApiIsidoreAuth  q) = Isidore.get lang limit Nothing  (Just q)


-- UNUSED
_flowCorpusApi :: ( FlowCmdM env err m)
               => Username -> Either CorpusName [CorpusId]
               -> TermType Lang
               -> Maybe Limit
               -> ApiQuery
               -> m CorpusId
_flowCorpusApi u n tt l q = do
  docs <- liftIO $ splitEvery 500 <$> getDataApi (_tt_lang tt) l q
  flowCorpus u n tt docs

------------------------------------------------------------------------

flowAnnuaire :: FlowCmdM env err m
             => Username
             -> Either CorpusName [CorpusId]
             -> (TermType Lang)
             -> FilePath
             -> m AnnuaireId
flowAnnuaire u n l filePath = do
  docs <- liftIO $ (( splitEvery 500 <$> deserialiseImtUsersFromFile filePath) :: IO [[HyperdataContact]])
  flow (Nothing :: Maybe HyperdataAnnuaire) u n l docs

-- UNUSED
_flowCorpusDebat :: FlowCmdM env err m
                 => Username -> Either CorpusName [CorpusId]
                 -> Limit -> FilePath
                 -> m CorpusId
_flowCorpusDebat u n l fp = do
  docs <- liftIO ( splitEvery 500
                 <$> take l
                 <$> readFile' fp
                 :: IO [[GD.GrandDebatReference ]]
                 )
  flowCorpus u n (Multi FR) (map (map toHyperdataDocument) docs)

flowCorpusFile :: FlowCmdM env err m
           => Username -> Either CorpusName [CorpusId]
           -> Limit -- Limit the number of docs (for dev purpose)
           -> TermType Lang -> FileFormat -> FilePath
           -> m CorpusId
flowCorpusFile u n l la ff fp = do
  docs <- liftIO ( splitEvery 500
                 <$> take l
                 <$> parseFile ff fp
                 )
  flowCorpus u n la (map (map toHyperdataDocument) docs)

-- TODO query with complex query
flowCorpusSearchInDatabase :: FlowCmdM env err m
                           => Username
                           -> Lang
                           -> Text
                           -> m CorpusId
flowCorpusSearchInDatabase u la q = do
  (_masterUserId, _masterRootId, cId) <- getOrMk_RootWithCorpus
                                           userMaster
                                           (Left "")
                                           (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchInDatabase cId (stemIt q)
  flowCorpusUser la u (Left q) (Nothing :: Maybe HyperdataCorpus) ids


-- UNUSED
_flowCorpusSearchInDatabaseApi :: FlowCmdM env err m
                               => Username
                               -> Lang
                               -> Text
                               -> m CorpusId
_flowCorpusSearchInDatabaseApi u la q = do
  (_masterUserId, _masterRootId, cId) <- getOrMk_RootWithCorpus
                                           userMaster
                                           (Left "")
                                           (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchInDatabase cId (stemIt q)
  flowCorpusUser la u (Left q) (Nothing :: Maybe HyperdataCorpus) ids

------------------------------------------------------------------------
-- | TODO improve the needed type to create/update a corpus
{- UNUSED
data UserInfo = Username Text
              | UserId   NodeId
data CorpusInfo = CorpusName Lang Text
                | CorpusId   Lang NodeId
-}

flow :: (FlowCmdM env err m, FlowCorpus a, MkCorpus c)
     => Maybe c
     -> Username
     -> Either CorpusName [CorpusId]
     -> TermType Lang
     -> [[a]]
     -> m CorpusId
flow c u cn la docs = do
  ids <- traverse (insertMasterDocs c la ) docs
  flowCorpusUser (la ^. tt_lang) u cn c (concat ids)

flowCorpus :: (FlowCmdM env err m, FlowCorpus a)
           => Username
           -> Either CorpusName [CorpusId]
           -> TermType Lang
           -> [[a]]
           -> m CorpusId
flowCorpus = flow (Nothing :: Maybe HyperdataCorpus)

------------------------------------------------------------------------
flowCorpusUser :: (FlowCmdM env err m, MkCorpus c)
               => Lang
               -> Username
               -> Either CorpusName [CorpusId]
               -> Maybe c
               -> [NodeId]
               -> m CorpusId
flowCorpusUser _l userName corpusName ctype ids = do
  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMk_RootWithCorpus userName corpusName ctype
  listId <- getOrMkList userCorpusId userId
  _cooc  <- mkNode NodeListCooc listId userId
  -- TODO: check if present already, ignore
  _ <- Doc.add userCorpusId ids

  _tId <- mkNode NodeTexts userCorpusId userId
  -- printDebug "Node Text Id" tId

  -- User List Flow
  (_masterUserId, _masterRootId, _masterCorpusId) <- getOrMk_RootWithCorpus userMaster (Left "") ctype
  --ngs         <- buildNgramsLists l 2 3 (StopSize 3) userCorpusId masterCorpusId
  -- _userListId <- flowList_DbRepo listId ngs
  --mastListId <- getOrMkList masterCorpusId masterUserId
  -- _ <- insertOccsUpdates userCorpusId mastListId
  -- printDebug "userListId" userListId
  -- User Graph Flow
  _ <- mkDashboard userCorpusId userId
  _ <- mkGraph  userCorpusId userId
  --_ <- mkPhylo  userCorpusId userId

  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId
  pure userCorpusId


insertMasterDocs :: ( FlowCmdM env err m
                    , FlowCorpus a
                    , MkCorpus   c
                    )
                 => Maybe c
                 -> TermType Lang
                 -> [a]
                 -> m [DocId]
insertMasterDocs c lang hs  =  do
  (masterUserId, _, masterCorpusId) <- getOrMk_RootWithCorpus userMaster (Left corpusMasterName) c

  -- TODO Type NodeDocumentUnicised
  let docs = map addUniqId hs
  ids <- insertDb masterUserId masterCorpusId docs
  let
    ids' = map reId ids
    documentsWithId = mergeData (toInserted ids) (Map.fromList $ map viewUniqId' docs)
  _ <- Doc.add masterCorpusId ids'
  -- TODO
  -- create a corpus with database name (CSV or PubMed)
  -- add documents to the corpus (create node_node link)
  -- this will enable global database monitoring

  -- maps :: IO Map Ngrams (Map NgramsType (Map NodeId Int))
  maps <- mapNodeIdNgrams
       <$> documentIdWithNgrams (extractNgramsT $ withLang lang documentsWithId) documentsWithId

  terms2id <- insertNgrams $ Map.keys maps
  -- to be removed
  let indexedNgrams = Map.mapKeys (indexNgrams terms2id) maps

  -- new
  lId      <- getOrMkList masterCorpusId masterUserId
  mapCgramsId <- listInsertDb lId toNodeNgramsW'
                $ map (first _ngramsTerms . second Map.keys)
                $ Map.toList maps
  -- insertDocNgrams
  _return <- insertNodeNodeNgrams2
           $ catMaybes [ NodeNodeNgrams2 <$> Just nId
                                         <*> getCgramsId mapCgramsId ngrams_type (_ngramsTerms terms)
                                         <*> Just (fromIntegral w :: Double)
                       | (terms, mapNgramsTypes) <- Map.toList maps
                       , (ngrams_type, mapNodeIdWeight) <- Map.toList mapNgramsTypes
                       , (nId, w) <- Map.toList mapNodeIdWeight
                       ]

  _cooc <- mkNode NodeListCooc lId masterUserId
  -- to be removed
  _   <- insertDocNgrams lId indexedNgrams

  pure ids'


withLang :: HasText a => TermType Lang
         -> [DocumentWithId a]
         -> TermType Lang
withLang (Unsupervised l n s m) ns = Unsupervised l n s m'
  where
    m' = case m of
      Nothing -> trace ("buildTries here" :: String)
              $ Just
              $ buildTries n ( fmap toToken $ uniText
                                            $ Text.intercalate " . "
                                            $ List.concat
                                            $ map hasText ns
                             )
      just_m -> just_m
withLang l _ = l



type CorpusName = Text


getOrMkRoot :: (HasNodeError err)
            => Username
            -> Cmd err (UserId, RootId)
getOrMkRoot username = do
  maybeUserId <- getUser username
  userId <- case maybeUserId of
        Nothing   -> nodeError NoUserFound
        Just user -> pure $ userLight_id user

  rootId' <- map _node_id <$> getRoot username

  rootId'' <- case rootId' of
        []  -> mkRoot username userId
        n   -> case length n >= 2 of
            True  -> nodeError ManyNodeUsers
            False -> pure rootId'

  rootId <- maybe (nodeError NoRootFound) pure (head rootId'')
  pure (userId, rootId)


getOrMk_RootWithCorpus :: (HasNodeError err, MkCorpus a)
                      => Username
                      -> Either CorpusName [CorpusId]
                      -> Maybe a
                      -> Cmd err (UserId, RootId, CorpusId)
getOrMk_RootWithCorpus username cName c = do
  (userId, rootId) <- getOrMkRoot username
  corpusId'' <- if username == userMaster
                  then do
                    ns <- getCorporaWithParentId rootId
                    pure $ map _node_id ns
                  else
                    pure $ fromRight [] cName

  corpusId' <- if corpusId'' /= []
                  then pure corpusId''
                  else do
                    c' <- mk (Just $ fromLeft "Default" cName) c rootId userId
                    _tId <- case head c' of
                              Nothing -> pure [0]
                              Just c'' -> mkNode NodeTexts c'' userId
                    pure c'

  corpusId <- maybe (nodeError NoCorpusFound) pure (head corpusId')

  pure (userId, rootId, corpusId)


------------------------------------------------------------------------
viewUniqId' :: UniqId a
            => a
            -> (HashId, a)
viewUniqId' d = maybe err (\h -> (h,d)) (view uniqId d)
      where
        err = panic "[ERROR] Database.Flow.toInsert"


toInserted :: [ReturnId]
           -> Map HashId ReturnId
toInserted =
  Map.fromList . map    (\r ->  (reUniqId r, r)    )
               . filter (\r -> reInserted r == True)

mergeData :: Map HashId ReturnId
          -> Map HashId a
          -> [DocumentWithId a]
mergeData rs = catMaybes . map toDocumentWithId . Map.toList
  where
    toDocumentWithId (sha,hpd) =
      DocumentWithId <$> fmap reId (lookup sha rs)
                     <*> Just hpd

------------------------------------------------------------------------

instance HasText HyperdataContact
  where
    hasText = undefined

instance ExtractNgramsT HyperdataContact
  where
    extractNgramsT l hc = filterNgramsT 255 <$> extract l hc
      where
        extract :: TermType Lang -> HyperdataContact
                -> Cmd err (Map Ngrams (Map NgramsType Int))
        extract _l hc' = do
          let authors = map text2ngrams
                     $ maybe ["Nothing"] (\a -> [a])
                     $ view (hc_who . _Just . cw_lastName) hc'

          pure $ Map.fromList $ [(a', Map.singleton Authors     1) | a' <- authors    ]

instance HasText HyperdataDocument
  where
    hasText h = catMaybes [ _hyperdataDocument_title    h
                          , _hyperdataDocument_abstract h
                          ]

instance ExtractNgramsT HyperdataDocument
  where
    extractNgramsT :: TermType Lang
                   -> HyperdataDocument
                   -> Cmd err (Map Ngrams (Map NgramsType Int))
    extractNgramsT lang hd = filterNgramsT 255 <$> extractNgramsT' lang hd
      where
        extractNgramsT' :: TermType Lang
                        -> HyperdataDocument
                       -> Cmd err (Map Ngrams (Map NgramsType Int))
        extractNgramsT' lang' doc = do
          let source    = text2ngrams
                        $ maybe "Nothing" identity
                        $ _hyperdataDocument_source doc

              institutes = map text2ngrams
                         $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))
                         $ _hyperdataDocument_institutes doc

              authors    = map text2ngrams
                         $ maybe ["Nothing"] (splitOn ", ")
                         $ _hyperdataDocument_authors doc

          terms' <- map text2ngrams
                 <$> map (intercalate " " . _terms_label)
                 <$> concat
                 <$> liftIO (extractTerms lang' $ hasText doc)

          pure $ Map.fromList $  [(source, Map.singleton Sources 1)]
                             <> [(i', Map.singleton Institutes  1) | i' <- institutes ]
                             <> [(a', Map.singleton Authors     1) | a' <- authors    ]
                             <> [(t', Map.singleton NgramsTerms 1) | t' <- terms'     ]

filterNgramsT :: Int -> Map Ngrams (Map NgramsType Int)
                     -> Map Ngrams (Map NgramsType Int)
filterNgramsT s ms = Map.fromList $ map (\a -> filter' s a) $ Map.toList ms
  where
    filter' s' (ng@(Ngrams t n),y) = case (Text.length t) < s' of
          True  -> (ng,y)
          False -> (Ngrams (Text.take s' t) n , y)


documentIdWithNgrams :: HasNodeError err
                     => (a
                     -> Cmd err (Map Ngrams (Map NgramsType Int)))
                     -> [DocumentWithId a]
                     -> Cmd err [DocumentIdWithNgrams a]
documentIdWithNgrams f = traverse toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ documentData         d
      pure   $ DocumentIdWithNgrams d e

