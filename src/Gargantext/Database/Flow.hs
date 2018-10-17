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
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.Flow
    where
import GHC.Base ((>>))
import Data.Maybe (Maybe(..))
import Gargantext.Core.Types (NodePoly(..))
import Gargantext.Prelude
import Gargantext.Database.Bashql (runCmd')
import Gargantext.Database.Node (Cmd(..), getRoot, mkRoot)
import Gargantext.Database.User (getUser, UserLight(..))
import Gargantext.Database.Node.Document.Import (insertDocuments)

--flow :: IO ()
flow = do
  masterUser <- runCmd' (getUser "gargantua")
  
  let masterUserId = case masterUser of
        Nothing   -> panic "no user"
        Just user -> userLight_id user
        
  root <- map node_id <$> runCmd' (getRoot masterUserId)
  
  root' <- case root of
        []  -> runCmd' (mkRoot masterUserId)
        un  -> case length un >= 2 of
                 True  -> panic "Error: more than 1 userNode / user"
                 False -> pure root

  printDebug "User Node : " root'
  
  pure ()
{-
  rootId       <- mk NodeUser gargantua_id "Node Gargantua"

  --folderId <- mk Folder parentId (Name "Data") (Descr "All corpora DATA here")
  folderId <- mk Folder rootId "Data"
  corpusId <- mk Corpus folderId (Name "WOS")  (Descr "WOS database description")
  
  docs <- parseDocuments WOS "doc/.."
  ids  <- add (Documents corpusId) docs

  user_id <- runCmd' (get RootUser "alexandre")
  rootUser_id <- runCmd' (getRootUser $ userLight_id user_id
  corpusId <- mk Corpus 
-}


