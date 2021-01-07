{-|
Module      : Gargantext.Database.Flow.List
Description : List Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE InstanceSigs            #-}

module Gargantext.Database.Action.Flow.List
    where

import Control.Concurrent
import Control.Lens (view, (^.), (+~), (%~), at)
import Control.Monad.Reader
import Data.Map (Map, toList)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Gargantext.API.Ngrams.Types (HasRepoSaver(..), NgramsElement(..), NgramsPatch(..), NgramsRepoElement(..), NgramsTablePatch(..), NgramsTerm(..), RepoCmdM, ne_ngrams, ngramsElementToRepo, r_history, r_state, r_version, repoVar)
import Gargantext.Core.Types (HasInvalidError(..), assertValid)
import Gargantext.Core.Types.Main (ListType(CandidateTerm))
import Gargantext.Core.Utils (something)
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Flow.Utils (DocumentIdWithNgrams(..))
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.NodeNgrams (NodeNgramsPoly(..), NodeNgramsW, listInsertDb, getCgramsId)
import Gargantext.Database.Query.Table.Node_NodeNgramsNodeNgrams
import Gargantext.Database.Schema.Ngrams (Ngrams(..), NgramsType(..))
import Gargantext.Database.Types
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Map.Strict.Patch as PM
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams

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
                -> Map Ngrams
                       (Map NgramsType 
                            (Map NodeId Int)
                       )
mapNodeIdNgrams = Map.unionsWith (Map.unionWith (Map.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams a
      -> Map Ngrams (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (Map.singleton nId)) $ documentNgrams d
      where
        nId = _index $ documentWithId d

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


-- NOTE
-- This is no longer part of the API.
-- This function is maintained for its usage in Database.Action.Flow.List.
-- If the given list of ngrams elements contains ngrams already in
-- the repo, they will be ignored.
putListNgrams :: (HasInvalidError err, RepoCmdM env err m)
              => NodeId
              -> TableNgrams.NgramsType
              -> [NgramsElement]
              -> m ()
putListNgrams _ _ [] = pure ()
putListNgrams nodeId ngramsType nes = putListNgrams' nodeId ngramsType m
  where
    m = Map.fromList $ map (\n -> (n ^. ne_ngrams, ngramsElementToRepo n)) nes

putListNgrams' :: (HasInvalidError err, RepoCmdM env err m)
               => NodeId
               -> TableNgrams.NgramsType
               -> Map NgramsTerm NgramsRepoElement
               -> m ()
putListNgrams' nodeId ngramsType ns = do
  -- printDebug "[putListNgrams'] nodeId" nodeId
  -- printDebug "[putListNgrams'] ngramsType" ngramsType
  -- printDebug "[putListNgrams'] ns" ns

  let p1 = NgramsTablePatch . PM.fromMap $ NgramsReplace Nothing . Just <$> ns
      (p0, p0_validity) = PM.singleton nodeId p1
      (p, p_validity) = PM.singleton ngramsType p0
  assertValid p0_validity
  assertValid p_validity
  {-
  -- TODO
  v <- currentVersion
  q <- commitStatePatch (Versioned v p)
  assert empty q
  -- What if another commit comes in between?
  -- Shall we have a blindCommitStatePatch? It would not ask for a version but just a patch.
  -- The modifyMVar_ would test the patch with applicable first.
  -- If valid the rest would be atomic and no merge is required.
  -}
  var <- view repoVar
  liftBase $ modifyMVar_ var $ \r -> do
    pure $ r & r_version +~ 1
             & r_history %~ (p :)
             & r_state . at ngramsType %~
               (Just .
                 (at nodeId %~
                   ( Just
                   . (<> ns)
                   . something
                   )
                 )
                 . something
               )
  saveRepo


saveRepo :: ( MonadReader env m, MonadBase IO m, HasRepoSaver env )
         => m ()
saveRepo = liftBase =<< view repoSaver
