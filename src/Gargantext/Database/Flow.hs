{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX



add :: Corpus -> [Documents] -> IO Int
if new id -> extractNgrams + extract Authors + extract Sources
Map (Ngrams, NodeId)
insert Ngrams -> NgramsId
Map (NgramsId, NodeId) -> insert

data NgramsType = Sources | Authors | Terms

nodes_ngrams : column type, column list


documents
sources
authors

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Database.Flow
    where

import Data.Maybe (Maybe(..))
import Gargantext.Prelude
import Gargantext.Database.Bashql (runCmd')
import Gargantext.Database.Node (Cmd(..), getRootUser)
import Gargantext.Database.User (getUser, UserLight(..))
import Gargantext.Database.Node.Document.Import (insertDocuments)

flow = do
  gargantua_id <- runCmd' (getUser "gargantua")
  -- createUser
  userNode <- case gargantua_id of
        Nothing     -> panic "no user"
        Just userId -> runCmd' (getRootUser $ userLight_id userId)
  
  case userNode of
        [] -> pure ()
        _  -> pure ()
  
  -- getOrMk
  --rootId       <- runCmd' (getNodeWith userId nodeType)
{-
  rootId       <- mk NodeUser gargantua_id "Node Gargantua"

  --folderId <- mk Folder parentId (Name "Data") (Descr "All corpora DATA here")
  folderId <- mk Folder rootId "Data"
  corpusId <- mk Corpus folderId (Name "WOS")  (Descr "WOS database description")
  
  docs <- parseDocuments WOS "doc/.."
  ids <- addDocuments corpusId docs

  user_id <- runCmd' (get RootUser "alexandre")
-}

















