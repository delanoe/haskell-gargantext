{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Flow -- (flowDatabase, ngrams2list)
    where

--import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
--import Gargantext.Core.Types
--import Gargantext.Database.Node.Contact (HyperdataContact(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Text (Text, splitOn, intercalate)
import Data.Tuple.Extra (both)
import Data.List (concat)
import GHC.Show (Show)
import Gargantext.Core.Types (NodePoly(..), ListType(..), listTypeId, Terms(..))
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Core.Types.Main
import Gargantext.Core (Lang(..))
import Gargantext.Database.Config (userMaster, userArbitrary, corpusMasterName)
import Gargantext.Database.Flow.Utils (insertToNodeNgrams)
import Gargantext.Text.Terms (extractTerms)
import Gargantext.Database.Node.Document.Add    (add)
import Gargantext.Database.Node.Document.Insert (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Root (getRoot)
import Gargantext.Database.Schema.Ngrams (insertNgrams, Ngrams(..), NgramsIndexed(..), indexNgrams,  NgramsType(..), text2ngrams, ngramsTypeId)
import Gargantext.Database.Schema.Node (mkRoot, mkCorpus, getOrMkList, mkGraph, mkDashboard, mkAnnuaire, getCorporaWithParentId, HasNodeError, NodeError(..), nodeError)
import Gargantext.Database.Schema.NodeNgram (NodeNgramPoly(..), insertNodeNgrams)
import Gargantext.Database.Schema.NodeNgramsNgrams (NodeNgramsNgramsPoly(..), insertNodeNgramsNgramsNew)
import Gargantext.Database.Schema.User (getUser, UserLight(..))
import Gargantext.Database.Types.Node (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Utils (Cmd)
import Gargantext.Text.Terms (TermType(..))
import Gargantext.Ext.IMT (toSchoolName)
import Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
import Gargantext.Prelude
import Gargantext.Text.Parsers (parseDocs, FileFormat)
import System.FilePath (FilePath)
import qualified Data.Map as DM


flowCorpus :: HasNodeError err => FileFormat -> FilePath -> CorpusName -> Cmd err CorpusId
flowCorpus ff fp cName = do
  hyperdataDocuments' <- map addUniqIdsDoc <$> liftIO (parseDocs ff fp)
  params <- flowInsert NodeCorpus hyperdataDocuments' cName
  flowCorpus' NodeCorpus hyperdataDocuments' params


flowInsert :: HasNodeError err => NodeType -> [HyperdataDocument] -> CorpusName
     -> Cmd err ([ReturnId], MasterUserId, MasterCorpusId, UserId, CorpusId)
flowInsert _nt hyperdataDocuments cName = do
  let hyperdataDocuments' = map (\h -> ToDbDocument h) hyperdataDocuments

  (masterUserId, _, masterCorpusId) <- subFlowCorpus userMaster corpusMasterName
  ids  <- insertDocuments masterUserId masterCorpusId NodeDocument hyperdataDocuments'

  (userId, _, userCorpusId) <- subFlowCorpus userArbitrary cName
  _ <- add userCorpusId (map reId ids)

  pure (ids, masterUserId, masterCorpusId, userId, userCorpusId)


flowAnnuaire :: HasNodeError err => FilePath -> Cmd err ()
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


flowCorpus' :: HasNodeError err
            => NodeType -> [HyperdataDocument]
            -> ([ReturnId], UserId, CorpusId, UserId, CorpusId)
            -> Cmd err CorpusId
flowCorpus' NodeCorpus hyperdataDocuments (ids,masterUserId,masterCorpusId, userId,userCorpusId) = do
--------------------------------------------------
  -- List Ngrams Flow
  userListId <- flowListUser userId userCorpusId
  printDebug "Working on User ListId : " userListId

  let documentsWithId = mergeData (toInserted ids) (toInsert hyperdataDocuments)
  -- printDebug "documentsWithId" documentsWithId
  docsWithNgrams <- documentIdWithNgrams extractNgramsT documentsWithId
  -- printDebug "docsWithNgrams" docsWithNgrams
  let maps            = mapNodeIdNgrams docsWithNgrams

  -- printDebug "maps" (maps)
  terms2id <- insertNgrams $ DM.keys maps
  let indexedNgrams = DM.mapKeys (indexNgrams terms2id) maps
  -- printDebug "inserted ngrams" indexedNgrams
  _             <- insertToNodeNgrams indexedNgrams

  listId2    <- flowList masterUserId masterCorpusId indexedNgrams
  printDebug "Working on ListId : " listId2
  --}
--------------------------------------------------
  _ <- mkDashboard userCorpusId userId
  _ <- mkGraph     userCorpusId userId

  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId

  pure userCorpusId
  -- del [corpusId2, corpusId]

flowCorpus' NodeAnnuaire _hyperdataDocuments (_ids,_masterUserId,_masterCorpusId,_userId,_userCorpusId) = undefined
flowCorpus' _ _ _ = undefined


type CorpusName = Text

subFlowCorpus :: HasNodeError err => Username -> CorpusName -> Cmd err (UserId, RootId, CorpusId)
subFlowCorpus username cName = do
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

  printDebug "(username, userId, rootId, corpusId)"
              (username, userId, rootId, corpusId)
  pure (userId, rootId, corpusId)


subFlowAnnuaire :: HasNodeError err => Username -> CorpusName -> Cmd err (UserId, RootId, CorpusId)
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

------------------------------------------------------------------------
toInsert :: [HyperdataDocument] -> Map HashId HyperdataDocument
toInsert = DM.fromList . map (\d -> (maybe err identity (_hyperdataDocument_uniqId d), d))
  where
    err = "Database.Flow.toInsert"

toInserted :: [ReturnId] -> Map HashId ReturnId
toInserted = DM.fromList . map    (\r ->  (reUniqId r, r)    )
                         . filter (\r -> reInserted r == True)

data DocumentWithId =
     DocumentWithId { documentId   :: !NodeId
                    , documentData :: !HyperdataDocument
                    } deriving (Show)

mergeData :: Map HashId ReturnId -> Map HashId HyperdataDocument -> [DocumentWithId]
mergeData rs = catMaybes . map toDocumentWithId . DM.toList
  where
    toDocumentWithId (hash,hpd) =
      DocumentWithId <$> fmap reId (lookup hash rs)
                     <*> Just hpd

------------------------------------------------------------------------
data DocumentIdWithNgrams =
     DocumentIdWithNgrams
     { documentWithId  :: !DocumentWithId
     , document_ngrams :: !(Map Ngrams (Map NgramsType Int))
     } deriving (Show)

-- TODO group terms
extractNgramsT :: HasNodeError err => HyperdataDocument -> Cmd err (Map Ngrams (Map NgramsType Int))
extractNgramsT doc = do
  let source    = text2ngrams $ maybe "Nothing" identity $ _hyperdataDocument_source doc
  let institutes = map text2ngrams $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))  $ _hyperdataDocument_institutes doc
  let authors    = map text2ngrams $ maybe ["Nothing"] (splitOn ", ") $ _hyperdataDocument_authors doc
  let leText = catMaybes [_hyperdataDocument_title doc, _hyperdataDocument_abstract doc]
  terms' <- map text2ngrams <$> map (intercalate " " . _terms_label) <$> concat <$> liftIO (extractTerms (Multi EN) leText)

  pure $ DM.fromList $  [(source, DM.singleton Sources 1)]
                     <> [(i', DM.singleton Institutes  1) | i' <- institutes ]
                     <> [(a', DM.singleton Authors     1) | a' <- authors    ]
                     <> [(t', DM.singleton NgramsTerms 1) | t' <- terms'     ]



documentIdWithNgrams :: HasNodeError err
                     => (HyperdataDocument -> Cmd err (Map Ngrams (Map NgramsType Int)))
                     -> [DocumentWithId]   -> Cmd err [DocumentIdWithNgrams]
documentIdWithNgrams f = mapM toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ documentData d
      pure $ DocumentIdWithNgrams d e

-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams] -> Map Ngrams (Map NgramsType (Map NodeId Int))
mapNodeIdNgrams = DM.unionsWith (DM.unionWith (DM.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams -> Map Ngrams (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (DM.singleton nId)) $ document_ngrams d
      where
        nId = documentId $ documentWithId d

------------------------------------------------------------------------
flowList :: HasNodeError err => UserId -> CorpusId
         -> Map NgramsIndexed (Map NgramsType (Map NodeId Int)) -> Cmd err ListId
flowList uId cId ngs = do
  -- printDebug "ngs:" ngs
  lId <- getOrMkList cId uId
  --printDebug "ngs" (DM.keys ngs)
  -- TODO add stemming equivalence of 2 ngrams
  -- TODO needs rework
  -- let groupEd = groupNgramsBy (\(NgramsT t1 n1) (NgramsT t2 n2) -> if (((==) t1 t2) && ((==) n1 n2)) then (Just (n1,n2)) else Nothing) ngs
  -- _ <- insertGroups lId groupEd

-- compute Candidate / Map
  is <- insertLists lId $ ngrams2list ngs
  printDebug "listNgrams inserted :" is

  pure lId

flowListUser :: HasNodeError err => UserId -> CorpusId -> Cmd err NodeId
flowListUser uId cId = getOrMkList cId uId

------------------------------------------------------------------------

{-
  TODO rework:
    * quadratic
    * DM.keys called twice
groupNgramsBy :: (NgramsT NgramsIndexed -> NgramsT NgramsIndexed -> Maybe (NgramsIndexed, NgramsIndexed))
              -> Map (NgramsT NgramsIndexed) (Map NodeId Int)
              -> Map NgramsIndexed NgramsIndexed
groupNgramsBy isEqual cId = DM.fromList $ catMaybes [ isEqual n1 n2 | n1 <- DM.keys cId, n2 <- DM.keys cId]
-}


-- TODO check: do not insert duplicates
insertGroups :: HasNodeError err => ListId -> Map NgramsIndexed NgramsIndexed -> Cmd err Int
insertGroups lId ngrs =
  insertNodeNgramsNgramsNew [ NodeNgramsNgrams lId ng1 ng2 (Just 1)
                            | (ng1, ng2) <- map (both _ngramsId) $ DM.toList ngrs
                            , ng1 /= ng2
                            ]

------------------------------------------------------------------------
ngrams2list :: Map NgramsIndexed (Map NgramsType a)
            -> [(ListType, (NgramsType,NgramsIndexed))]
ngrams2list m =
  [ (GraphList, (t, ng))
  | (ng, tm) <- DM.toList m
  , t <- DM.keys tm
  ]

-- | TODO: weight of the list could be a probability
insertLists :: HasNodeError err => ListId -> [(ListType, (NgramsType, NgramsIndexed))] -> Cmd err Int
insertLists lId lngs = insertNodeNgrams [ NodeNgram lId (_ngramsId ng) Nothing (ngramsTypeId ngt) (fromIntegral $ listTypeId l) 1
                     | (l,(ngt, ng)) <- lngs
                   ]
------------------------------------------------------------------------
