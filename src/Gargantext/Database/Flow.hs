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
import Data.Tuple.Extra (both, second)
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
import Gargantext.Database.Schema.Ngrams (insertNgrams, Ngrams(..), NgramsT(..), NgramsIndexed(..), indexNgramsT,  NgramsType(..), text2ngrams)
import Gargantext.Database.Schema.Node (mkRoot, mkCorpus, getOrMkList, mkGraph, mkDashboard, mkAnnuaire, getCorporaWithParentId, HasNodeError)
import Gargantext.Database.Schema.NodeNgram (NodeNgramPoly(..), insertNodeNgrams)
import Gargantext.Database.Schema.NodeNgramsNgrams (NodeNgramsNgramsPoly(..), insertNodeNgramsNgramsNew)
import Gargantext.Database.Schema.User (getUser, UserLight(..))
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import Gargantext.Database.Types.Node (NodeType(..), NodeId)
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


flowInsert :: NodeType -> [HyperdataDocument] -> CorpusName
     -> Cmd err ([ReturnId], MasterUserId, MasterCorpusId, UserId, CorpusId)
flowInsert _nt hyperdataDocuments cName = do
  let hyperdataDocuments' = map (\h -> ToDbDocument h) hyperdataDocuments

  (masterUserId, _, masterCorpusId) <- subFlowCorpus userMaster corpusMasterName
  ids  <- insertDocuments masterUserId masterCorpusId NodeDocument hyperdataDocuments'
  
  (userId, _, userCorpusId) <- subFlowCorpus userArbitrary cName
  _ <- add userCorpusId (map reId ids)
  
  pure (ids, masterUserId, masterCorpusId, userId, userCorpusId)


flowAnnuaire :: FilePath -> Cmd err ()
flowAnnuaire filePath = do
  contacts <- liftIO $ deserialiseImtUsersFromFile filePath
  ps <- flowInsertAnnuaire "Annuaire" $ map (\h-> ToDbContact h) $ map addUniqIdsContact contacts
  printDebug "length annuaire" ps


flowInsertAnnuaire :: CorpusName -> [ToDbData]
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
  indexedNgrams <- indexNgrams maps
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

subFlowCorpus :: Username -> CorpusName -> Cmd err (UserId, RootId, CorpusId)
subFlowCorpus username cName = do
  maybeUserId <- getUser username

  let userId = case maybeUserId of
        Nothing   -> panic "Error: User does not exist (yet)"
        -- mk NodeUser gargantua_id "Node Gargantua"
        Just user -> userLight_id user

  rootId' <- map _node_id <$> getRoot username

  rootId'' <- case rootId' of
        []  -> mkRoot username userId
        n   -> case length n >= 2 of
            True  -> panic "Error: more than 1 userNode / user"
            False -> pure rootId'
  let rootId = maybe (panic "error rootId") identity (head rootId'')

  corpusId'' <- if username == userMaster
                  then do
                    ns <- getCorporaWithParentId rootId
                    pure $ map _node_id ns
                  else
                    pure []

  corpusId' <- if corpusId'' /= []
                  then pure corpusId''
                  else mkCorpus (Just cName) Nothing rootId userId

  let corpusId = maybe (panic "error corpusId") identity (head corpusId')

  printDebug "(username, userId, rootId, corpusId)"
              (username, userId, rootId, corpusId)
  pure (userId, rootId, corpusId)


subFlowAnnuaire :: Username -> CorpusName -> Cmd err (UserId, RootId, CorpusId)
subFlowAnnuaire username _cName = do
  maybeUserId <- getUser username

  let userId = case maybeUserId of
        Nothing   -> panic "Error: User does not exist (yet)"
        -- mk NodeUser gargantua_id "Node Gargantua"
        Just user -> userLight_id user

  rootId' <- map _node_id <$> getRoot username

  rootId'' <- case rootId' of
        []  -> mkRoot username userId
        n   -> case length n >= 2 of
            True  -> panic "Error: more than 1 userNode / user"
            False -> pure rootId'
  let rootId = maybe (panic "error rootId") identity (head rootId'')

  corpusId' <- mkAnnuaire rootId userId
  
  let corpusId = maybe (panic "error corpusId") identity (head corpusId')

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
     , document_ngrams :: !(Map (NgramsT Ngrams) Int)
     } deriving (Show)

-- TODO add Terms (Title + Abstract)
-- add f :: Text -> Text
-- newtype Ngrams = Ngrams Text
-- TODO group terms
extractNgramsT :: HyperdataDocument -> Cmd err (Map (NgramsT Ngrams) Int)
extractNgramsT doc = do
  
  let source    = text2ngrams $ maybe "Nothing" identity $ _hyperdataDocument_source doc
  let institutes = map text2ngrams $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))  $ _hyperdataDocument_institutes doc
  let authors    = map text2ngrams $ maybe ["Nothing"] (splitOn ", ") $ _hyperdataDocument_authors doc
  let leText = catMaybes [_hyperdataDocument_title doc, _hyperdataDocument_abstract doc]
  terms' <- map text2ngrams <$> map (intercalate " " . _terms_label) <$> concat <$> liftIO (extractTerms (Multi EN) leText)

  pure $ DM.fromList $  [(NgramsT Sources source, 1)]
                     <> [(NgramsT Institutes  i' , 1)| i' <- institutes ]
                     <> [(NgramsT Authors     a' , 1)| a' <- authors    ]
                     <> [(NgramsT NgramsTerms t' , 1)| t' <- terms'     ]




documentIdWithNgrams :: (HyperdataDocument -> Cmd err (Map (NgramsT Ngrams) Int))
                     -> [DocumentWithId]   -> Cmd err [DocumentIdWithNgrams]
documentIdWithNgrams f = mapM toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ documentData d
      pure $ DocumentIdWithNgrams d e

-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams] -> Map (NgramsT Ngrams) (Map NodeId Int)
mapNodeIdNgrams ds = DM.map (DM.fromListWith (+)) $ DM.fromListWith (<>) xs
  where
    xs  = [(ng, [(nId, i)]) | (nId, n2i') <- n2i ds, (ng, i) <- DM.toList n2i']
    n2i = map (\d -> ((documentId . documentWithId) d, document_ngrams d))

indexNgrams :: Map (NgramsT Ngrams       ) (Map NodeId Int)
  -> Cmd err (Map (NgramsT NgramsIndexed) (Map NodeId Int))
indexNgrams ng2nId = do
  terms2id <- insertNgrams (map _ngramsT $ DM.keys ng2nId)
  pure $ DM.mapKeys (indexNgramsT terms2id) ng2nId


------------------------------------------------------------------------
------------------------------------------------------------------------
flowList :: HasNodeError err => UserId -> CorpusId -> Map (NgramsT NgramsIndexed) (Map NodeId Int) -> Cmd err ListId
flowList uId cId ngs = do
  -- printDebug "ngs:" ngs
  lId <- getOrMkList cId uId
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

flowListUser :: HasNodeError err => UserId -> CorpusId -> Cmd err Int
flowListUser uId cId = getOrMkList cId uId

------------------------------------------------------------------------

groupNgramsBy :: (NgramsT NgramsIndexed -> NgramsT NgramsIndexed -> Maybe (NgramsIndexed, NgramsIndexed))
              -> Map (NgramsT NgramsIndexed) (Map NodeId Int)
              -> Map NgramsIndexed NgramsIndexed
groupNgramsBy isEqual cId = DM.fromList $ catMaybes [ isEqual n1 n2 | n1 <- DM.keys cId, n2 <- DM.keys cId]



-- TODO check: do not insert duplicates
insertGroups :: ListId -> Map NgramsIndexed NgramsIndexed -> Cmd err Int
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
insertLists :: ListId -> [(ListType,NgramsIndexed)] -> Cmd err Int
insertLists lId lngs =
  insertNodeNgrams [ NodeNgram Nothing lId ngr (fromIntegral $ listTypeId l) (listTypeId l)
                     | (l,ngr) <- map (second _ngramsId) lngs
                   ]

------------------------------------------------------------------------
------------------------------------------------------------------------
