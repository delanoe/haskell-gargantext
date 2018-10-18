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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.Flow
    where
import System.FilePath (FilePath)
import GHC.Base ((>>))
import Data.Maybe (Maybe(..))
import Gargantext.Core.Types (NodePoly(..))
import Gargantext.Prelude
import Gargantext.Database.Bashql (runCmd', del)
import Gargantext.Database.Node (Cmd(..), getRoot, mkRoot, mkCorpus)
import Gargantext.Database.User (getUser, UserLight(..), Username)
import Gargantext.Database.Node.Document.Insert (insertDocuments, ReturnId(reId))
import Gargantext.Database.Node.Document.Add    (add)
import Gargantext.Text.Parsers (parseDocs, FileFormat(WOS))

type UserId = Int
type RootId = Int
type CorpusId = Int

subFlow :: Username -> IO (UserId, RootId, CorpusId)
subFlow username = do
  maybeUserId <- runCmd' (getUser username)
  
  let userId = case maybeUserId of
        Nothing   -> panic "Error: User does not exist (yet)" -- mk NodeUser gargantua_id "Node Gargantua"
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


-- flow :: FilePath -> IO ()
flow fp = do

  (masterUserId, _, corpusId) <- subFlow "gargantua"

  docs <- parseDocs WOS fp
  ids  <- runCmd' $ insertDocuments masterUserId corpusId docs
  printDebug "Docs IDs : " ids

  idsRepeat  <- runCmd' $ insertDocuments masterUserId corpusId docs
  printDebug "Docs IDs : " idsRepeat
  
  (userId, rootId, corpusId2) <- subFlow "alexandre"

  inserted <- runCmd' $ add corpusId2 (map reId ids)
  printDebug "Inserted : " inserted

  -- runCmd' (del [corpusId2, corpusId])

{-
  ids  <- add (Documents corpusId) docs

  user_id <- runCmd' (get RootUser "alexandre")
  rootUser_id <- runCmd' (getRootUser $ userLight_id user_id
  corpusId <- mk Corpus 
-}

