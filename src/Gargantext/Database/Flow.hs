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

-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}

module Gargantext.Database.Flow -- (flowDatabase, ngrams2list)
    where

import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)
--import Gargantext.Database.Node.Contact (HyperdataContact(..))
import Data.Map (Map, lookup, toList)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Monoid
import Data.Text (Text, splitOn, intercalate)
import qualified Data.Text as Text
import Data.List (concat)
import GHC.Show (Show)
import Gargantext.Core.Types (NodePoly(..), Terms(..))
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Core.Types.Main
import Gargantext.Core (Lang(..))
import Gargantext.Database.Config (userMaster, corpusMasterName)
import Gargantext.Database.Flow.Utils (insertToNodeNgrams)
--import Gargantext.Database.Metrics.TFICF (getTficf)
import Gargantext.Text.Terms (extractTerms)
--import Gargantext.Text.Metrics.TFICF (Tficf(..))
import qualified Gargantext.Database.Node.Document.Add  as Doc  (add)
import Gargantext.Database.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Root (getRoot)
import Gargantext.Database.Schema.Ngrams -- (insertNgrams, Ngrams(..), NgramsIndexed(..), indexNgrams,  NgramsType(..), text2ngrams, ngramsTypeId)
import Gargantext.Database.Schema.Node -- (mkRoot, mkCorpus, getOrMkList, mkGraph, mkDashboard, mkAnnuaire, getCorporaWithParentId, HasNodeError, NodeError(..), nodeError)
-- import Gargantext.Database.Schema.NodeNgram (NodeNgramPoly(..), insertNodeNgrams)
--import Gargantext.Database.Schema.NodeNgramsNgrams (NodeNgramsNgramsPoly(..), insertNodeNgramsNgramsNew)
--import Gargantext.Database.Metrics.Count (getNgramsElementsWithParentNodeId)
import Gargantext.Database.Schema.User (getUser, UserLight(..))
import Gargantext.Database.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Utils (Cmd, CmdM)
import Gargantext.Text.Terms (TermType(..))
import Gargantext.Ext.IMT (toSchoolName)
--import Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
import Gargantext.Prelude
import Gargantext.Text.List
import Gargantext.Text.Parsers (parseDocs, FileFormat)
import System.FilePath (FilePath)
import Gargantext.API.Ngrams (HasRepoVar)
import Servant (ServantErr)
import Gargantext.API.Ngrams (NgramsElement, putListNgrams, RepoCmdM)
--import Gargantext.Database.Schema.User (insertUsers, simpleUser, gargantuaUser)
import qualified Data.Map as DM

type FlowCmdM env err m =
  ( CmdM     env err m
  , RepoCmdM env err m
  , HasNodeError err
  , HasRepoVar env
  )


flowCorpus :: FlowCmdM env ServantErr m
           => Username -> FileFormat -> FilePath -> CorpusName -> m CorpusId
flowCorpus userName ff fp corpusName = do

  -- Master Flow
  docs <- map addUniqIdsDoc <$> liftIO (parseDocs ff fp)
  -- ChunkAlong needed for big corpora
  -- TODO add LANG as parameter
  -- TODO uniformize language of corpus
                                   -- TODO ChunkAlong is not the right function here
                                   -- chunkAlong 10 10 [1..15] == [1..10]
                                   -- BUG: what about the rest (divMod 15 10)?
                                   -- but if temporary enables big corpora insert for tests
                                   -- TODO: chunkAlongNoRest or chunkAlongWithRest
                                   -- default: NoRest
  ids  <- mapM insertMasterDocs $ chunkAlong 10000 10000 docs

  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMkRootWithCorpus userName corpusName
  -- TODO: check if present already, ignore
  _ <- Doc.add userCorpusId $ concat ids

  -- User List Flow
  (_masterUserId, _masterRootId, masterCorpusId) <- getOrMkRootWithCorpus userMaster ""
  -- /!\ this extract NgramsTerms Only
  ngs <- buildNgramsList userCorpusId masterCorpusId
  --printDebug "ngs" ngs
  -- TODO getNgramsElement of NgramsType...
  --ngs <- getNgramsElementsWithParentNodeId masterCorpusId
  userListId   <- flowList userId userCorpusId ngs
  printDebug "userListId" userListId

  -- User Graph Flow
  _ <- mkGraph     userCorpusId userId

  -- User Dashboard Flow
  _ <- mkDashboard userCorpusId userId

  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId

  pure userCorpusId


insertMasterDocs :: FlowCmdM env ServantErr m
                => [HyperdataDocument] -> m [DocId]
insertMasterDocs hs  =  do

  let hyperdataDocuments' = map (\h -> ToDbDocument h) hs
  
  -- TODO put in State Monad
  (masterUserId, _, masterCorpusId) <- getOrMkRootWithCorpus userMaster corpusMasterName
  ids <- insertDocuments masterUserId masterCorpusId NodeDocument hyperdataDocuments'

  let documentsWithId = mergeData (toInserted ids) (toInsert hs)
  docsWithNgrams <- documentIdWithNgrams extractNgramsT documentsWithId

  let maps            = mapNodeIdNgrams docsWithNgrams

  --printDebug "maps" (maps)
  terms2id <- insertNgrams $ DM.keys maps
  let indexedNgrams = DM.mapKeys (indexNgrams terms2id) maps
  --printDebug "inserted ngrams" indexedNgrams
  _             <- insertToNodeNgrams indexedNgrams
  pure $ map reId ids



getUserCorpusNgrams :: FlowCmdM env ServantErr m
                => CorpusId -> m [Ngrams]
getUserCorpusNgrams = undefined




type CorpusName = Text

getOrMkRootWithCorpus :: HasNodeError err
              => Username -> CorpusName
              -> Cmd err (UserId, RootId, CorpusId)
getOrMkRootWithCorpus username cName = do
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

  corpusId'' <- if username == userMaster
                  then do
                    ns <- getCorporaWithParentId rootId
                    pure $ map _node_id ns
                  else
                    pure []

  corpusId' <- if corpusId'' /= []
                  then pure corpusId''
                  else mkCorpus (Just cName) Nothing rootId userId

  corpusId <- maybe (nodeError NoCorpusFound) pure (head corpusId')

  pure (userId, rootId, corpusId)




------------------------------------------------------------------------
toInsert :: [HyperdataDocument] -> Map HashId HyperdataDocument
toInsert = DM.fromList . map (\d -> (maybe err identity (_hyperdataDocument_uniqId d), d))
  where
    err = "[ERROR] Database.Flow.toInsert"

toInserted :: [ReturnId] -> Map HashId ReturnId
toInserted = DM.fromList . map    (\r ->  (reUniqId r, r)    )
                         . filter (\r -> reInserted r == True)

data DocumentWithId = DocumentWithId
  { documentId   :: !NodeId
  , documentData :: !HyperdataDocument
  } deriving (Show)

mergeData :: Map HashId ReturnId
          -> Map HashId HyperdataDocument
          -> [DocumentWithId]
mergeData rs = catMaybes . map toDocumentWithId . DM.toList
  where
    toDocumentWithId (hash,hpd) =
      DocumentWithId <$> fmap reId (lookup hash rs)
                     <*> Just hpd

------------------------------------------------------------------------
data DocumentIdWithNgrams = DocumentIdWithNgrams
  { documentWithId  :: !DocumentWithId
  , document_ngrams :: !(Map Ngrams (Map NgramsType Int))
  } deriving (Show)

-- TODO group terms

extractNgramsT :: HasNodeError err
               => HyperdataDocument
               -> Cmd err (Map Ngrams (Map NgramsType Int))
extractNgramsT hd = filterNgramsT 255 <$> extractNgramsT' hd


extractNgramsT' :: HasNodeError err
               => HyperdataDocument
               -> Cmd err (Map Ngrams (Map NgramsType Int))
extractNgramsT' doc = do
  let source    = text2ngrams
                $ maybe "Nothing" identity
                $ _hyperdataDocument_source doc

      institutes = map text2ngrams
                 $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))
                 $ _hyperdataDocument_institutes doc

      authors    = map text2ngrams
                 $ maybe ["Nothing"] (splitOn ", ")
                 $ _hyperdataDocument_authors doc

      leText = catMaybes [ _hyperdataDocument_title    doc
                         , _hyperdataDocument_abstract doc
                         ]

  terms' <- map text2ngrams
         <$> map (intercalate " " . _terms_label)
         <$> concat
         <$> liftIO (extractTerms (Multi EN) leText)

  pure $ DM.fromList $  [(source, DM.singleton Sources 1)]
                     <> [(i', DM.singleton Institutes  1) | i' <- institutes ]
                     <> [(a', DM.singleton Authors     1) | a' <- authors    ]
                     <> [(t', DM.singleton NgramsTerms 1) | t' <- terms'     ]


filterNgramsT :: Int -> Map Ngrams (Map NgramsType Int)
                     -> Map Ngrams (Map NgramsType Int)
filterNgramsT s ms = DM.fromList $ map (\a -> filter' s a) $ DM.toList ms
  where
    filter' s' (ng@(Ngrams t n),y) = case (Text.length t) < s' of
          True  -> (ng,y)
          False -> (Ngrams (Text.take s' t) n , y)


documentIdWithNgrams :: HasNodeError err
                     => (HyperdataDocument
                     -> Cmd err (Map Ngrams (Map NgramsType Int)))
                     -> [DocumentWithId]
                     -> Cmd err [DocumentIdWithNgrams]
documentIdWithNgrams f = mapM toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ documentData d
      pure $ DocumentIdWithNgrams d e



-- FLOW LIST
-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams]
                -> Map Ngrams (Map NgramsType (Map NodeId Int))
mapNodeIdNgrams = DM.unionsWith (DM.unionWith (DM.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams
      -> Map Ngrams (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (DM.singleton nId)) $ document_ngrams d
      where
        nId = documentId $ documentWithId d

------------------------------------------------------------------------
flowListBase :: FlowCmdM env err m
             => ListId -> Map NgramsType [NgramsElement]
             -> m ()
flowListBase lId ngs = do
-- compute Candidate / Map
  mapM_ (\(typeList, ngElmts) -> putListNgrams lId typeList ngElmts) $ toList ngs

flowList :: FlowCmdM env err m => UserId -> CorpusId
         -> Map NgramsType [NgramsElement]
         -> m ListId
flowList uId cId ngs = do
  lId <- getOrMkList cId uId
  printDebug "listId flowList" lId
  flowListBase lId ngs
  pure lId


------------------------------------------------------------------------
------------------------------------------------------------------------

{-
-- | Annuaire

flowAnnuaire :: FlowCmdM env ServantErr m => FilePath -> m ()
flowAnnuaire filePath = do
  contacts <- liftIO $ deserialiseImtUsersFromFile filePath
  ps <- flowInsertAnnuaire "Annuaire" $ map (\h-> ToDbContact h) $ map addUniqIdsContact contacts
  printDebug "length annuaire" ps


flowInsertAnnuaire :: HasNodeError err => CorpusName -> [ToDbData]
                    -> Cmd err ([ReturnId], UserId, CorpusId, UserId, CorpusId)
flowInsertAnnuaire name children = do

  (masterUserId, _, masterCorpusId) <- subFlowCorpus userMaster corpusMasterName
  ids  <- insertDocuments masterUserId masterCorpusId NodeContact children

  (userId, _, userCorpusId) <- subFlowAnnuaire userArbitrary name
  _ <- add userCorpusId (map reId ids)

  printDebug "AnnuaireID" userCorpusId

  pure (ids, masterUserId, masterCorpusId, userId, userCorpusId)


subFlowAnnuaire :: HasNodeError err =>
  Username -> CorpusName -> Cmd err (UserId, RootId, CorpusId)
subFlowAnnuaire username _cName = do
  maybeUserId <- getUser username

  userId <- case maybeUserId of
        Nothing   -> nodeError NoUserFound
        -- mk NodeUser gargantua_id "Node Gargantua"
        Just user -> pure $ userLight_id user

  rootId' <- map _node_id <$> getRoot username

  rootId'' <- case rootId' of
        []  -> mkRoot username userId
        n   -> case length n >= 2 of
            True  -> nodeError ManyNodeUsers
            False -> pure rootId'
  rootId <- maybe (nodeError NoRootFound) pure (head rootId'')

  corpusId' <- mkAnnuaire rootId userId

  corpusId <- maybe (nodeError NoCorpusFound) pure (head corpusId')

  printDebug "(username, userId, rootId, corpusId)"
              (username, userId, rootId, corpusId)
  pure (userId, rootId, corpusId)

-}
