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
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE InstanceSigs            #-}

module Gargantext.Database.Action.Flow.List
    where

import Control.Monad (mapM_)
import qualified Data.List as List
import qualified Data.Map  as Map
import Data.Map (Map, toList)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Text (Text)

import Gargantext.API.Ngrams (NgramsElement(..), NgramsTerm(..), putListNgrams)
import Gargantext.Core.Flow.Types
import Gargantext.Core.Types.Main (ListType(CandidateTerm))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.NodeNgrams (NodeNgramsPoly(..), NodeNgramsW, listInsertDb, getCgramsId)
import Gargantext.Database.Schema.Ngrams -- (insertNgrams, Ngrams(..), NgramsIndexed(..), indexNgrams,  NgramsType(..), text2ngrams, ngramsTypeId)
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Query.Table.Node_NodeNgramsNodeNgrams
import Gargantext.Prelude

-- FLOW LIST
-- 1. select specific terms of the corpus when compared with others langs
-- (for now, suppose english)
-- 2. select specific terms of the corpus when compared with others corpora (same database)
-- 3. select clusters of terms (generic and specific)

{-
data FlowList = FlowListLang
              | FlowListTficf
              | FlowListSpeGen


flowList_Tficf :: UserCorpusId
               -> MasterCorpusId
               -> NgramsType
               -> (Text -> Text)
               -> Cmd err (Map Text (Double, Set Text))
flowList_Tficf u m nt f = do

  u' <- Map.filter (\s -> Set.size s > 1) <$> getNodesByNgramsUser   u nt
  m' <- Map.filter (\s -> Set.size s > 1) <$> getNodesByNgramsMaster u m

  pure $ sortTficf Down
       $ toTficfData (countNodesByNgramsWith f u')
                     (countNodesByNgramsWith f m')

flowList_Tficf' :: UserCorpusId
               -> MasterCorpusId
               -> NgramsType
               -> Cmd err (Map Text (Double, Set Text))
flowList_Tficf' u m nt f = do

  u' <- Map.filter (\s -> Set.size s > 1) <$> getNodesByNgramsUser   u nt
  m' <- Map.filter (\s -> Set.size s > 1) <$> getNodesByNgramsMaster u m

  pure $ sortTficf Down
       $ toTficfData (countNodesByNgramsWith f u')
                     (countNodesByNgramsWith f m')

-}


-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams a]
                -> Map Ngrams (Map NgramsType (Map NodeId Int))
mapNodeIdNgrams = Map.unionsWith (Map.unionWith (Map.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams a
      -> Map Ngrams (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (Map.singleton nId)) $ documentNgrams d
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
  let toInsert = catMaybes [ (,) <$> (getCgramsId mapCgramsId ntype <$> (unNgramsTerm <$> parent))
                                 <*>  getCgramsId mapCgramsId ntype ngram
                           | (ntype, ngs') <- Map.toList ngs
                           , NgramsElement (NgramsTerm ngram) _ _ _ _ parent _ <- ngs'
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
       (NgramsElement (NgramsTerm ngrams_terms') _size list_type _occ _root _parent _children) <- elms
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
                             -> putListNgrams lId typeList ngElmts) (toList ngs)

------------------------------------------------------------------------
------------------------------------------------------------------------
