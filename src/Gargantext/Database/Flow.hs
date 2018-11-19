{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


Map (NgramsId, NodeId) -> insert
data NgramsType = Sources | Authors | Terms
nodes_ngrams : column type, column list

documents
sources
authors

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.Flow (flowDatabase)
    where

import GHC.Show (Show)
import System.FilePath (FilePath)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Text (Text, splitOn)
import Data.Map (Map)
import Data.Tuple.Extra (both, second)
import qualified Data.Map as DM

import Gargantext.Core.Types (NodePoly(..), ListType(..), listId)
import Gargantext.Database.Bashql (runCmd')--, del)
import Gargantext.Database.Ngrams (insertNgrams, Ngrams(..), NgramsT(..), NgramsIndexed(..), indexNgramsT, ngramsTypeId, NgramsType(..), text2ngrams)
import Gargantext.Database.Node (getRoot, mkRoot, mkCorpus, Cmd(..), mkList)
import Gargantext.Database.Node.Document.Add    (add)
import Gargantext.Database.Node.Document.Insert (insertDocuments, ReturnId(..), addUniqIds)
import Gargantext.Database.NodeNgram (NodeNgramPoly(..), insertNodeNgrams)
import Gargantext.Database.NodeNgramsNgrams (NodeNgramsNgramsPoly(..), insertNodeNgramsNgramsNew)
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import Gargantext.Database.User (getUser, UserLight(..), Username)
import Gargantext.Prelude
import Gargantext.Text.Parsers (parseDocs, FileFormat)
import Gargantext.Ext.IMT (toSchoolName)

type UserId = Int
type RootId = Int
type CorpusId = Int

flowDatabase :: FileFormat -> FilePath -> CorpusName -> IO [Int]
flowDatabase ff fp cName = do
  
  -- Corus Flow
  (masterUserId, _, corpusId) <- subFlow "gargantua" "Big Corpus"

  -- Documents Flow
  hyperdataDocuments <- map addUniqIds <$> parseDocs ff fp

  --printDebug "hyperdataDocuments" hyperdataDocuments

  ids  <- runCmd' $ insertDocuments masterUserId corpusId hyperdataDocuments
  --printDebug "Docs IDs : " (ids)
  idsRepeat  <- runCmd' $ insertDocuments masterUserId corpusId hyperdataDocuments
  --printDebug "Repeated Docs IDs : " (length ids)
  
  -- Ngrams Flow
  -- todo: flow for new documents only
  let tids = toInserted ids
  --printDebug "toInserted ids" (length tids, tids)

  let tihs = toInsert hyperdataDocuments
  --printDebug "toInsert hyperdataDocuments" (length tihs, tihs)

  let documentsWithId = mergeData (toInserted ids) (toInsert hyperdataDocuments)
  -- printDebug "documentsWithId" documentsWithId

  let docsWithNgrams  = documentIdWithNgrams extractNgramsT documentsWithId
  printDebug "docsWithNgrams" docsWithNgrams
  {-
  
  let maps            = mapNodeIdNgrams docsWithNgrams
  printDebug "maps" (maps)
  
  indexedNgrams <- runCmd' $ indexNgrams maps
  printDebug "inserted ngrams" indexedNgrams
  _             <- runCmd' $ insertToNodeNgrams indexedNgrams

  -- List Flow
  listId2 <- runCmd' $ listFlow masterUserId corpusId indexedNgrams
  printDebug "list id : " listId2

  printDebug "Docs IDs : " (length idsRepeat)

  -}
  (_, _, corpusId2) <- subFlow "user1" cName
  {-
  inserted <- runCmd' $ add corpusId2 (map reId ids)
  printDebug "Inserted : " (length inserted)
  -}
  pure [corpusId2, corpusId]

  --runCmd' $ del [corpusId2, corpusId]

type CorpusName = Text

subFlow :: Username -> CorpusName -> IO (UserId, RootId, CorpusId)
subFlow username cName = do
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

------------------------------------------------------------------------

type HashId   = Text
type NodeId   = Int
type ListId   = Int

toInsert :: [HyperdataDocument] -> Map HashId HyperdataDocument
toInsert = DM.fromList . map (\d -> (hash (_hyperdataDocument_uniqId d), d))
  where
    hash = maybe "Error" identity

toInserted :: [ReturnId] -> Map HashId ReturnId
toInserted rs = DM.fromList $ map    (\r ->  (reUniqId r, r)    )
                            $ filter (\r -> reInserted r == True) rs

data DocumentWithId =
     DocumentWithId { documentId   :: NodeId
                    , documentData :: HyperdataDocument
                    } deriving (Show)

mergeData :: Map HashId ReturnId -> Map HashId HyperdataDocument -> [DocumentWithId]
mergeData rs hs = map (\(hash,hpd) -> DocumentWithId (lookup' hash rs) hpd) $ DM.toList hs
                  where
                    lookup' h xs = maybe (panic $ "Database.Flow.mergeData: Error with " <> h) reId (DM.lookup h rs)

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
listFlow uId cId ng = do
  lId <- maybe (panic "mkList error") identity <$> head <$> mkList cId uId
  -- TODO add stemming equivalence of 2 ngrams
  let groupEd = groupNgramsBy (\(NgramsT t1 n1) (NgramsT t2 n2) -> if (((==) t1 t2) && ((==) n1 n2)) then (Just (n1,n2)) else Nothing) ng
  _ <- insertGroups lId groupEd

-- compute Candidate / Map
  let lists = ngrams2list ng
  _ <- insertLists lId lists

  pure lId

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
                            ]

------------------------------------------------------------------------
-- TODO: verify NgramsT lost here
ngrams2list :: Map (NgramsT NgramsIndexed) (Map NodeId Int) -> Map ListType NgramsIndexed
ngrams2list = DM.fromList . zip (repeat Candidate) . map (\(NgramsT _lost_t ng) -> ng) . DM.keys

-- | TODO: weight of the list could be a probability
insertLists :: ListId -> Map ListType NgramsIndexed -> Cmd Int
insertLists lId list2ngrams =
  insertNodeNgrams [ NodeNgram Nothing lId ngr (fromIntegral $ listId l) (listId l)
                     | (l,ngr) <- map (second _ngramsId)   $ DM.toList list2ngrams
                   ]

------------------------------------------------------------------------
------------------------------------------------------------------------

