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

module Gargantext.Database.Flow
    where
import System.FilePath (FilePath)
import Data.Maybe (Maybe(..))
import Data.Text (Text, unpack)
import Data.Map (Map)
import qualified Data.Map as DM
import GHC.Generics (Generic)

import Gargantext.Core.Types (NodePoly(..))
import Gargantext.Prelude
import Gargantext.Database.Bashql (runCmd', del)
import Gargantext.Database.Types.Node (Node(..), HyperdataDocument(..))
import Gargantext.Database.Node (getRoot, mkRoot, mkCorpus, Cmd(..))
import Gargantext.Database.User (getUser, UserLight(..), Username)
import Gargantext.Database.Node.Document.Insert (insertDocuments, ReturnId(..), addUniqIds)
import Gargantext.Database.Node.Document.Add    (add)
import Gargantext.Database.NodeNgram (NodeNgramPoly(..), insertNodeNgrams)
import Gargantext.Text.Parsers (parseDocs, FileFormat(WOS))
import Gargantext.Database.Ngram (insertNgrams, Ngrams(..), NgramsT(..), NgramsIndexed(..), indexNgramsT, ngramsTypeId)

type UserId = Int
type RootId = Int
type CorpusId = Int

subFlow :: Username -> IO (UserId, RootId, CorpusId)
subFlow username = do
  maybeUserId <- runCmd' (getUser username)

  let userId = case maybeUserId of
        Nothing   -> panic "Error: User does not exist (yet)" 
        -- mk NodeUser gargantua_id "Node Gargantua"
        Just user -> userLight_id user

  rootId' <- map _node_id <$> runCmd' (getRoot userId)

  rootId'' <- case rootId' of
        []  -> runCmd' (mkRoot userId)
        un  -> case length un >= 2 of
                 True  -> panic "Error: more than 1 userNode / user"
                 False -> pure rootId'
  let rootId = maybe (panic "error rootId") identity (head rootId'')

  corpusId' <- runCmd' $ mkCorpus (Just "Corpus WOS") Nothing rootId userId
  let corpusId = maybe (panic "error corpusId") identity (head corpusId')

  printDebug "(username, userId, rootId, corpusId"
              (username, userId, rootId, corpusId)
  pure (userId, rootId, corpusId)


flow :: FilePath -> IO Int
flow fp = do

  (masterUserId, _, corpusId) <- subFlow "gargantua"

  docs <- map addUniqIds <$> parseDocs WOS fp
  ids  <- runCmd' $ insertDocuments masterUserId corpusId docs
  printDebug "Docs IDs : " ids

  idsRepeat  <- runCmd' $ insertDocuments masterUserId corpusId docs
  printDebug "Docs IDs : " idsRepeat

  (_, _, corpusId2) <- subFlow "alexandre"

  inserted <- runCmd' $ add corpusId2 (map reId ids)
  printDebug "Inserted : " inserted

  runCmd' $ del [corpusId2, corpusId]

----------------------------------------------------------------
type HashId   = Text
type NodeId   = Int
type ToInsert = Map HashId HyperdataDocument
type Inserted = Map HashId ReturnId

toInsert :: [HyperdataDocument] -> Map HashId HyperdataDocument
toInsert = DM.fromList . map (\d -> (hash (_hyperdataDocument_uniqIdBdd d), d))
  where
    hash = maybe "Error" identity

toInserted :: [ReturnId] -> Map HashId ReturnId
toInserted rs = DM.fromList $ map    (\r ->  (reUniqId r, r)    )
                            $ filter (\r -> reInserted r == True) rs

data DocumentWithId = DocumentWithId { documentId   :: NodeId
                             , documentData :: HyperdataDocument
                             }


mergeData :: Map HashId ReturnId -> Map HashId HyperdataDocument -> [DocumentWithId]
mergeData rs hs = map (\(hash,r) -> DocumentWithId (reId r) (lookup' hash hs)) $ DM.toList rs
  where
    lookup' h xs = maybe (panic $ "Error with " <> h) identity (DM.lookup h xs)

data DocumentIdWithNgrams = DocumentIdWithNgrams { documentWithId  :: DocumentWithId
                                                 , document_ngrams :: Map (NgramsT Ngrams)Int
                                                 }


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
insertToNodeNgrams m = insertNodeNgrams $ [ NodeNgram Nothing nId  ((_ngramsId    . _ngramsT   ) ng)
                                                  (fromIntegral n) ((ngramsTypeId . _ngramsType) ng)

                                          | (ng, nId2int) <- DM.toList m
                                          , (nId, n)      <- DM.toList nId2int
                                          ]

-- mk ListGroup
-- groupBy fun
-- insertInto NodeNgramsNgrams

-- compute Candidate / Map
-- add column typelist
-- insertNodeNodeNgram

-- get data of NgramsTable
-- post :: update NodeNodeNgrams
-- group ngrams

