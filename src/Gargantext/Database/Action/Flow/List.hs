{-|
Module      : Gargantext.Database.Flow.List
Description : List Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

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

module Gargantext.Database.Action.Flow.List
    where

import Control.Monad (mapM_)
import Data.Map (Map, toList)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Text (Text)
import Gargantext.API.Ngrams (NgramsElement(..), putListNgrams)
import Gargantext.Core.Types.Main (ListType(CandidateTerm))
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Admin.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Schema.Ngrams -- (insertNgrams, Ngrams(..), NgramsIndexed(..), indexNgrams,  NgramsType(..), text2ngrams, ngramsTypeId)
import Gargantext.Database.Schema.NodeNgrams (NodeNgramsPoly(..), NodeNgramsW, listInsertDb, getCgramsId)
import Gargantext.Database.Schema.Node_NodeNgramsNodeNgrams -- (insert_Node_NodeNgrams_NodeNgrams, Node_NodeNgrams_NodeNgrams(..))
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Map  as Map


-- FLOW LIST
-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams a]
                -> Map Ngrams (Map NgramsType (Map NodeId Int))
mapNodeIdNgrams = Map.unionsWith (Map.unionWith (Map.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams a
      -> Map Ngrams (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (Map.singleton nId)) $ document_ngrams d
      where
        nId = documentId $ documentWithId d

------------------------------------------------------------------------
flowList_DbRepo :: FlowCmdM env err m
         => ListId
         -> Map NgramsType [NgramsElement]
         -> m ListId
flowList_DbRepo lId ngs = do
  -- printDebug "listId flowList" lId
  mapCgramsId <- listInsertDb lId toNodeNgramsW (Map.toList ngs)
  let toInsert = catMaybes [ (,) <$> (getCgramsId mapCgramsId ntype <$> parent)
                                 <*>  getCgramsId mapCgramsId ntype ngram
                           | (ntype, ngs') <- Map.toList ngs
                           , NgramsElement ngram _ _ _ _ parent _ <- ngs'
                           ]
  -- Inserting groups of ngrams
  _r <- insert_Node_NodeNgrams_NodeNgrams
     $ map (\(a,b) -> Node_NodeNgrams_NodeNgrams lId a b Nothing) toInsert
  listInsert lId ngs
  --trace (show $ List.filter (\n -> _ne_ngrams n == "versatile") $ List.concat $ Map.elems ngs) $ listInsert lId ngs
  pure lId
------------------------------------------------------------------------
------------------------------------------------------------------------

toNodeNgramsW :: ListId
              -> [(NgramsType, [NgramsElement])]
              -> [NodeNgramsW]
toNodeNgramsW l ngs = List.concat $ map (toNodeNgramsW'' l) ngs
  where
    toNodeNgramsW'' :: ListId
                  -> (NgramsType, [NgramsElement])
                  -> [NodeNgramsW]
    toNodeNgramsW'' l' (ngrams_type, elms) =
      [ NodeNgrams Nothing l' list_type ngrams_terms' ngrams_type Nothing Nothing Nothing 0 |
       (NgramsElement ngrams_terms' _size list_type _occ _root _parent _children) <- elms
      ]

toNodeNgramsW' :: ListId
               -> [(Text, [NgramsType])]
               -> [NodeNgramsW]
toNodeNgramsW' l'' ngs = [ NodeNgrams Nothing l'' CandidateTerm terms ngrams_type Nothing Nothing Nothing 0
                         | (terms, ngrams_types) <- ngs
                         , ngrams_type <- ngrams_types
                         ]


listInsert :: FlowCmdM env err m
             => ListId
             -> Map NgramsType [NgramsElement]
             -> m ()
listInsert lId ngs = mapM_ (\(typeList, ngElmts)
                             -> putListNgrams lId typeList ngElmts
                             ) $ toList ngs

------------------------------------------------------------------------
------------------------------------------------------------------------
