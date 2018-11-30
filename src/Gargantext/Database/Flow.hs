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

module Gargantext.Database.Flow -- (flowDatabase, ngrams2list)
    where

import GHC.Show (Show)
import System.FilePath (FilePath)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Text (Text, splitOn)
import Data.Map (Map, lookup)
import Data.Tuple.Extra (both, second)
import qualified Data.Map as DM

import Gargantext.Core.Types (NodePoly(..), ListType(..), listTypeId)
import Gargantext.Database.Bashql (runCmd') -- , del)
import Gargantext.Database.Config (userMaster, userArbitrary, corpusMasterName)
import Gargantext.Database.Ngrams (insertNgrams, Ngrams(..), NgramsT(..), NgramsIndexed(..), indexNgramsT, ngramsTypeId, NgramsType(..), text2ngrams)
import Gargantext.Database.Node (getRoot, mkRoot, mkCorpus, Cmd(..), mkList, mkGraph, mkDashboard, mkAnnuaire)
import Gargantext.Database.Types.Node (NodeType(..))
import Gargantext.Database.Node.Document.Add    (add)
import Gargantext.Database.Node.Document.Insert (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.NodeNgram (NodeNgramPoly(..), insertNodeNgrams)
import Gargantext.Database.NodeNgramsNgrams (NodeNgramsNgramsPoly(..), insertNodeNgramsNgramsNew)
import Gargantext.Database.Types.Node (HyperdataDocument(..))
--import Gargantext.Database.Node.Contact (HyperdataContact(..))
import Gargantext.Database.User (getUser, UserLight(..), Username)
import Gargantext.Ext.IMT (toSchoolName)
import Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
import Gargantext.Prelude
import Gargantext.Text.Parsers (parseDocs, FileFormat)

type UserId   = Int
type MasterUserId = Int

type RootId   = Int
type CorpusId = Int
type MasterCorpusId = Int

flowDatabase :: FileFormat -> FilePath -> CorpusName -> IO CorpusId
flowDatabase ff fp cName = do
  -- Corpus Flow
  hyperdataDocuments <- map addUniqIdsDoc <$> parseDocs ff fp
  params <- flowInsert NodeCorpus hyperdataDocuments cName
  flowCorpus NodeCorpus hyperdataDocuments params


flowInsert :: NodeType -> [HyperdataDocument] -> CorpusName
     -> IO ([ReturnId], MasterUserId, MasterCorpusId, UserId, CorpusId)
flowInsert _nt hyperdataDocuments cName = do
  let hyperdataDocuments' = map (\h -> ToDbDocument h) hyperdataDocuments

  (masterUserId, _, masterCorpusId) <- subFlowCorpus userMaster corpusMasterName
  ids  <- runCmd' $ insertDocuments masterUserId masterCorpusId hyperdataDocuments'
  
  (userId, _, userCorpusId) <- subFlowCorpus userArbitrary cName
  _ <- runCmd' $ add userCorpusId (map reId ids)
  
  pure (ids, masterUserId, masterCorpusId, userId, userCorpusId)


flowAnnuaire :: FilePath -> IO ()
flowAnnuaire filePath = do
  contacts <- deserialiseImtUsersFromFile filePath
  ps <- flowInsertAnnuaire "Annuaire" $ map (\h-> ToDbContact h) $ map addUniqIdsContact contacts 
  printDebug "length annuaire" (ps)

--{-

flowInsertAnnuaire :: CorpusName
                                -> [ToDbData]
                                -> IO ([ReturnId], UserId, CorpusId, UserId, CorpusId)
flowInsertAnnuaire name children = do

  (masterUserId, _, masterCorpusId) <- subFlowCorpus userMaster corpusMasterName
  ids  <- runCmd' $ insertDocuments masterUserId masterCorpusId children
  
  (userId, _, userCorpusId) <- subFlowAnnuaire userArbitrary name
  _ <- runCmd' $ add userCorpusId (map reId ids)
  
  printDebug "AnnuaireID" userCorpusId

  pure (ids, masterUserId, masterCorpusId, userId, userCorpusId)


--}

--{-
flowCorpus :: NodeType
                        -> [HyperdataDocument]
                        -> ([ReturnId], UserId, CorpusId, UserId, CorpusId)
                        -> IO CorpusId
flowCorpus NodeCorpus hyperdataDocuments (ids,masterUserId,masterCorpusId, userId,userCorpusId) = do
--}
--------------------------------------------------
  -- List Ngrams Flow
  userListId <- runCmd' $ listFlowUser userId userCorpusId
  printDebug "Working on User ListId : " userListId
  
  let documentsWithId = mergeData (toInserted ids) (toInsert hyperdataDocuments)
  -- printDebug "documentsWithId" documentsWithId
  let docsWithNgrams  = documentIdWithNgrams extractNgramsT documentsWithId
  -- printDebug "docsWithNgrams" docsWithNgrams
  let maps            = mapNodeIdNgrams docsWithNgrams
  
  -- printDebug "maps" (maps)
  indexedNgrams <- runCmd' $ indexNgrams maps
  -- printDebug "inserted ngrams" indexedNgrams
  _             <- runCmd' $ insertToNodeNgrams indexedNgrams
  
  listId2    <- runCmd' $ listFlow masterUserId masterCorpusId indexedNgrams
  printDebug "Working on ListId : " listId2
  --}

--------------------------------------------------
  _ <- runCmd' $ mkDashboard userCorpusId userId
  _ <- runCmd' $ mkGraph     userCorpusId userId
  
  -- Annuaire Flow
  -- _ <- runCmd' $ mkAnnuaire  rootUserId userId

  pure userCorpusId
  -- runCmd' $ del [corpusId2, corpusId]

flowCorpus NodeAnnuaire _hyperdataDocuments (_ids,_masterUserId,_masterCorpusId,_userId,_userCorpusId) = undefined
flowCorpus _ _ _ = undefined


type CorpusName = Text

subFlowCorpus :: Username -> CorpusName -> IO (UserId, RootId, CorpusId)
subFlowCorpus username cName = do
  maybeUserId <- runCmd' (getUser username)

  let userId = case maybeUserId of
        Nothing   -> panic "Error: User does not exist (yet)"
        -- mk NodeUser gargantua_id "Node Gargantua"
        Just user -> userLight_id user

  rootId' <- map _node_id <$> runCmd' (getRoot userId)

  rootId'' <- case rootId' of
        []  -> runCmd' (mkRoot username userId)
        n   -> case length n >= 2 of
            True  -> panic "Error: more than 1 userNode / user"
            False -> pure rootId'
  let rootId = maybe (panic "error rootId") identity (head rootId'')

  corpusId' <- runCmd' $ mkCorpus (Just cName) Nothing rootId userId
  let corpusId = maybe (panic "error corpusId") identity (head corpusId')

  printDebug "(username, userId, rootId, corpusId)"
              (username, userId, rootId, corpusId)
  pure (userId, rootId, corpusId)


subFlowAnnuaire :: Username -> CorpusName -> IO (UserId, RootId, CorpusId)
subFlowAnnuaire username _cName = do
  maybeUserId <- runCmd' (getUser username)

  let userId = case maybeUserId of
        Nothing   -> panic "Error: User does not exist (yet)"
        -- mk NodeUser gargantua_id "Node Gargantua"
        Just user -> userLight_id user

  rootId' <- map _node_id <$> runCmd' (getRoot userId)

  rootId'' <- case rootId' of
        []  -> runCmd' (mkRoot username userId)
        n   -> case length n >= 2 of
            True  -> panic "Error: more than 1 userNode / user"
            False -> pure rootId'
  let rootId = maybe (panic "error rootId") identity (head rootId'')

  corpusId' <- runCmd' $ mkAnnuaire rootId userId
  let corpusId = maybe (panic "error corpusId") identity (head corpusId')

  printDebug "(username, userId, rootId, corpusId)"
              (username, userId, rootId, corpusId)
  pure (userId, rootId, corpusId)



------------------------------------------------------------------------

type HashId   = Text
type NodeId   = Int
type ListId   = Int

toInsert :: [HyperdataDocument] -> Map HashId HyperdataDocument
toInsert = DM.fromList . map (\d -> (maybe err identity (_hyperdataDocument_uniqId d), d))
  where
    err = "Database.Flow.toInsert"

toInserted :: [ReturnId] -> Map HashId ReturnId
toInserted = DM.fromList . map    (\r ->  (reUniqId r, r)    )
                         . filter (\r -> reInserted r == True)

data DocumentWithId =
     DocumentWithId { documentId   :: NodeId
                    , documentData :: HyperdataDocument
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
     { documentWithId  :: DocumentWithId
     , document_ngrams :: Map (NgramsT Ngrams) Int
     } deriving (Show)

-- TODO add Terms (Title + Abstract)
-- add f :: Text -> Text
-- newtype Ngrams = Ngrams Text
extractNgramsT :: HyperdataDocument -> Map (NgramsT Ngrams) Int
extractNgramsT doc = DM.fromList $  [(NgramsT Sources source, 1)]
                                 <> [(NgramsT Institutes i' , 1)| i' <- institutes ]
                                 <> [(NgramsT Authors    a' , 1)| a' <- authors    ]
  where
    source    = text2ngrams $ maybe "Nothing" identity $ _hyperdataDocument_source doc
    institutes = map text2ngrams $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))  $ _hyperdataDocument_institutes doc
    authors    = map text2ngrams $ maybe ["Nothing"] (splitOn ", ") $ _hyperdataDocument_authors doc
    -- TODO group terms




documentIdWithNgrams :: (HyperdataDocument -> Map (NgramsT Ngrams) Int)
                     -> [DocumentWithId]   -> [DocumentIdWithNgrams]
documentIdWithNgrams f = map (\d -> DocumentIdWithNgrams d ((f . documentData) d))

-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams] -> Map (NgramsT Ngrams) (Map NodeId Int)
mapNodeIdNgrams ds = DM.map (DM.fromListWith (+)) $ DM.fromListWith (<>) xs
  where
    xs  = [(ng, [(nId, i)]) | (nId, n2i') <- n2i ds, (ng, i) <- DM.toList n2i']
    n2i = map (\d -> ((documentId . documentWithId) d, document_ngrams d))

indexNgrams :: Map (NgramsT Ngrams       ) (Map NodeId Int)
       -> Cmd (Map (NgramsT NgramsIndexed) (Map NodeId Int))
indexNgrams ng2nId = do
  terms2id <- insertNgrams (map _ngramsT $ DM.keys ng2nId)
  pure $ DM.mapKeys (indexNgramsT terms2id) ng2nId


insertToNodeNgrams :: Map (NgramsT NgramsIndexed) (Map NodeId Int) -> Cmd Int
insertToNodeNgrams m = insertNodeNgrams [ NodeNgram Nothing nId  ((_ngramsId    . _ngramsT   ) ng)
                                                (fromIntegral n) ((ngramsTypeId . _ngramsType) ng)
                                          | (ng, nId2int) <- DM.toList m
                                          , (nId, n)      <- DM.toList nId2int
                                        ]


------------------------------------------------------------------------
------------------------------------------------------------------------
listFlow :: UserId -> CorpusId -> Map (NgramsT NgramsIndexed) (Map NodeId Int) -> Cmd ListId
listFlow uId cId ngs = do
  -- printDebug "ngs:" ngs
  lId <- maybe (panic "mkList error") identity <$> head <$> mkList cId uId
  --printDebug "ngs" (DM.keys ngs)
  -- TODO add stemming equivalence of 2 ngrams
  let groupEd = groupNgramsBy (\(NgramsT t1 n1) (NgramsT t2 n2) -> if (((==) t1 t2) && ((==) n1 n2)) then (Just (n1,n2)) else Nothing) ngs
  _ <- insertGroups lId groupEd

-- compute Candidate / Map
  let lists = ngrams2list ngs
  -- printDebug "lists:" lists
  
  is <- insertLists lId lists
  printDebug "listNgrams inserted :" is

  pure lId

listFlowUser :: UserId -> CorpusId -> Cmd [Int]
listFlowUser uId cId = mkList cId uId

------------------------------------------------------------------------

groupNgramsBy :: (NgramsT NgramsIndexed -> NgramsT NgramsIndexed -> Maybe (NgramsIndexed, NgramsIndexed))
              -> Map (NgramsT NgramsIndexed) (Map NodeId Int)
              -> Map NgramsIndexed NgramsIndexed
groupNgramsBy isEqual cId = DM.fromList $ catMaybes [ isEqual n1 n2 | n1 <- DM.keys cId, n2 <- DM.keys cId]



-- TODO check: do not insert duplicates
insertGroups :: ListId -> Map NgramsIndexed NgramsIndexed -> Cmd Int
insertGroups lId ngrs =
  insertNodeNgramsNgramsNew [ NodeNgramsNgrams lId ng1 ng2 (Just 1)
                              | (ng1, ng2) <- map (both _ngramsId) $ DM.toList ngrs
                              , ng1 /= ng2
                            ]

------------------------------------------------------------------------
-- TODO: verify NgramsT lost here
ngrams2list :: Map (NgramsT NgramsIndexed) (Map NodeId Int) -> [(ListType,NgramsIndexed)]
ngrams2list = zip (repeat CandidateList) . map (\(NgramsT _lost_t ng) -> ng) . DM.keys

-- | TODO: weight of the list could be a probability
insertLists :: ListId -> [(ListType,NgramsIndexed)] -> Cmd Int
insertLists lId lngs =
  insertNodeNgrams [ NodeNgram Nothing lId ngr (fromIntegral $ listTypeId l) (listTypeId l)
                     | (l,ngr) <- map (second _ngramsId) lngs
                   ]

------------------------------------------------------------------------
------------------------------------------------------------------------

