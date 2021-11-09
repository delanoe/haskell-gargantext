{-|
Module      : Gargantext.API.Ngrams
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams API

-- | TODO
get ngrams filtered by NgramsType
add get 

-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

module Gargantext.API.Ngrams
  ( TableNgramsApi
  , TableNgramsApiGet
  , TableNgramsApiPut

  , getTableNgrams
  , setListNgrams
  --, rmListNgrams TODO fix before exporting
  , apiNgramsTableCorpus
  , apiNgramsTableDoc

  , NgramsTablePatch
  , NgramsTableMap

  , NgramsTerm(..)

  , NgramsElement(..)
  , mkNgramsElement

  , RootParent(..)

  , MSet
  , mSetFromList
  , mSetToList

  , Repo(..)
  , r_version
  , r_state
  , r_history
  , NgramsRepoElement(..)
  , saveNodeStory
  , initRepo

  , TabType(..)

  , QueryParamR
  , TODO

  -- Internals
  , getNgramsTableMap
  , dumpJsonTableMap
  , tableNgramsPull
  , tableNgramsPut

  , Version
  , Versioned(..)
  , VersionedWithCount(..)
  , currentVersion
  , listNgramsChangedSince
  )
  where

import Control.Concurrent
import Control.Lens ((.~), view, (^.), (^..), (+~), (%~), (.~), sumOf, at, _Just, Each(..), (%%~), mapped, ifolded, withIndex)
import Control.Monad.Reader
import Data.Aeson hiding ((.=))
import Data.Either (Either(..))
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord (Down(..))
import Data.Patch.Class (Action(act), Transformable(..), ours)
import Data.Swagger hiding (version, patch)
import Data.Text (Text, isInfixOf, unpack)
import Data.Text.Lazy.IO as DTL
import Formatting (hprint, int, (%))
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Admin.Types (HasSettings)
import Gargantext.API.Job
import Gargantext.API.Ngrams.Types
import Gargantext.API.Prelude
import Gargantext.Core.NodeStory
import Gargantext.Core.Types (ListType(..), NodeId, ListId, DocId, Limit, Offset, TODO, assertValid, HasInvalidError)
import Gargantext.API.Ngrams.Tools
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Metrics.NgramsByNode (getOccByNgramsOnlyFast')
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Admin.Types.Node (NodeType(..))
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Database.Query.Table.Ngrams hiding (NgramsType(..), ngramsType, ngrams_terms)
import Gargantext.Database.Query.Table.Node (getNode)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Schema.Node (node_id, node_parent_id, node_user_id)
import Gargantext.Prelude hiding (log)
import Gargantext.Prelude.Clock (hasTime, getTime)
import Prelude (error)
import Servant hiding (Patch)
import Servant.Job.Async (JobFunction(..), serveJobsAPI)
import System.IO (stderr)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Data.Aeson.Text as DAT
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.Patch as PM
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Gargantext.API.Metrics as Metrics
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams

{-
-- TODO sequences of modifications (Patchs)
type NgramsIdPatch = Patch NgramsId NgramsPatch

ngramsPatch :: Int -> NgramsPatch
ngramsPatch n = NgramsPatch (DM.fromList [(1, StopTerm)]) (Set.fromList [n]) Set.empty

toEdit :: NgramsId -> NgramsPatch -> Edit NgramsId NgramsPatch
toEdit n p = Edit n p
ngramsIdPatch :: Patch NgramsId NgramsPatch
ngramsIdPatch = fromList $ catMaybes $ reverse [ replace (1::NgramsId) (Just $ ngramsPatch 1) Nothing
                                       , replace (1::NgramsId) Nothing (Just $ ngramsPatch 2)
                                       , replace (2::NgramsId) Nothing (Just $ ngramsPatch 2)
                                       ]

-- applyPatchBack :: Patch -> IO Patch
-- isEmptyPatch = Map.all (\x -> Set.isEmpty (add_children x) && Set.isEmpty ... )
-}
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

{-
-- TODO: Replace.old is ignored which means that if the current list
-- `MapTerm` and that the patch is `Replace CandidateTerm StopTerm` then
-- the list is going to be `StopTerm` while it should keep `MapTerm`.
-- However this should not happen in non conflicting situations.
mkListsUpdate :: NgramsType -> NgramsTablePatch -> [(NgramsTypeId, NgramsTerm, ListTypeId)]
mkListsUpdate nt patches =
  [ (ngramsTypeId nt, ng, listTypeId lt)
  | (ng, patch) <- patches ^.. ntp_ngrams_patches . ifolded . withIndex
  , lt <- patch ^.. patch_list . new
  ]

mkChildrenGroups :: (PatchSet NgramsTerm -> Set NgramsTerm)
                 -> NgramsType
                 -> NgramsTablePatch
                 -> [(NgramsTypeId, NgramsParent, NgramsChild)]
mkChildrenGroups addOrRem nt patches =
  [ (ngramsTypeId nt, parent, child)
  | (parent, patch) <- patches ^.. ntp_ngrams_patches . ifolded . withIndex
  , child <- patch ^.. patch_children . to addOrRem . folded
  ]
-}

------------------------------------------------------------------------

saveNodeStory :: ( MonadReader env m, MonadBase IO m, HasNodeStorySaver env )
         => m ()
saveNodeStory = liftBase =<< view hasNodeStorySaver


listTypeConflictResolution :: ListType -> ListType -> ListType
listTypeConflictResolution _ _ = undefined -- TODO Use Map User ListType


ngramsStatePatchConflictResolution
  :: TableNgrams.NgramsType
  -> NgramsTerm
  -> ConflictResolutionNgramsPatch
ngramsStatePatchConflictResolution _ngramsType _ngramsTerm
  = (ours, (const ours, ours), (False, False))
                             -- (False, False) mean here that Mod has always priority.
                             -- (True, False) <- would mean priority to the left (same as ours).
  -- undefined {- TODO think this through -}, listTypeConflictResolution)




-- Current state:
--   Insertions are not considered as patches,
--   they do not extend history,
--   they do not bump version.
insertNewOnly :: a -> Maybe b -> a
insertNewOnly m = maybe m (const $ error "insertNewOnly: impossible")
  -- TODO error handling

{- unused
-- TODO refactor with putListNgrams
copyListNgrams :: RepoCmdM env err m
               => NodeId -> NodeId -> NgramsType
               -> m ()
copyListNgrams srcListId dstListId ngramsType = do
  var <- view repoVar
  liftBase $ modifyMVar_ var $
    pure . (r_state . at ngramsType %~ (Just . f . something))
  saveNodeStory
  where
    f :: Map NodeId NgramsTableMap -> Map NodeId NgramsTableMap
    f m = m & at dstListId %~ insertNewOnly (m ^. at srcListId)

-- TODO refactor with putListNgrams
-- The list must be non-empty!
-- The added ngrams must be non-existent!
addListNgrams :: RepoCmdM env err m
              => NodeId -> NgramsType
              -> [NgramsElement] -> m ()
addListNgrams listId ngramsType nes = do
  var <- view repoVar
  liftBase $ modifyMVar_ var $
    pure . (r_state . at ngramsType . _Just . at listId . _Just <>~ m)
  saveNodeStory
  where
    m = Map.fromList $ (\n -> (n ^. ne_ngrams, n)) <$> nes
-}

-- | TODO: incr the Version number
-- && should use patch
-- UNSAFE

setListNgrams ::  HasNodeStory env err m
              => NodeId
              -> TableNgrams.NgramsType
              -> Map NgramsTerm NgramsRepoElement
              -> m ()
setListNgrams listId ngramsType ns = do
  printDebug "[setListNgrams]" (listId, ngramsType)
  getter <- view hasNodeStory
  var <- liftBase $ (getter ^. nse_getter) [listId]
  liftBase $ modifyMVar_ var $
    pure . ( unNodeStory
           . at listId . _Just
            . a_state
              . at ngramsType
              .~ Just ns
           )
  saveNodeStory


currentVersion :: HasNodeStory env err m
               => ListId -> m Version
currentVersion listId = do
  nls <- getRepo' [listId]
  pure $ nls ^. unNodeStory . at listId . _Just . a_version


newNgramsFromNgramsStatePatch :: NgramsStatePatch' -> [Ngrams]
newNgramsFromNgramsStatePatch p =
  [ text2ngrams (unNgramsTerm n)
  | (n,np) <- p ^.. _PatchMap
                -- . each . _PatchMap
                . each . _NgramsTablePatch
                . _PatchMap . ifolded . withIndex
  , _ <- np ^.. patch_new . _Just
  ]




commitStatePatch :: HasNodeStory env err m
                 => ListId
                 ->    Versioned NgramsStatePatch'
                 -> m (Versioned NgramsStatePatch')
commitStatePatch listId (Versioned p_version p) = do
  printDebug "[commitStatePatch]" listId
  var <- getNodeStoryVar [listId]
  vq' <- liftBase $ modifyMVar var $ \ns -> do
    let
      a = ns ^. unNodeStory . at listId . _Just
      q = mconcat $ take (a ^. a_version - p_version) (a ^. a_history)
      (p', q') = transformWith ngramsStatePatchConflictResolution p q
      a' = a & a_version +~ 1
             & a_state   %~ act p'
             & a_history %~ (p' :)

    {-
    -- Ideally we would like to check these properties. However:
    -- * They should be checked only to debug the code. The client data
    --   should be able to trigger these.
    -- * What kind of error should they throw (we are in IO here)?
    -- * Should we keep modifyMVar?
    -- * Should we throw the validation in an Exception, catch it around
    --   modifyMVar and throw it back as an Error?
    assertValid $ transformable p q
    assertValid $ applicable p' (r ^. r_state)
    -}
    printDebug "[commitStatePatch] a version" (a ^. a_version)
    printDebug "[commitStatePatch] a' version" (a' ^. a_version)
    pure ( ns & unNodeStory . at listId .~ (Just a')
         , Versioned (a' ^. a_version) q'
         )
  saveNodeStory
  -- Save new ngrams
  _ <- insertNgrams (newNgramsFromNgramsStatePatch p)

  pure vq'



-- This is a special case of tableNgramsPut where the input patch is empty.
tableNgramsPull :: HasNodeStory env err m
                => ListId
                -> TableNgrams.NgramsType
                -> Version
                -> m (Versioned NgramsTablePatch)
tableNgramsPull listId ngramsType p_version = do
  printDebug "[tableNgramsPull]" (listId, ngramsType)
  var <- getNodeStoryVar [listId]
  r <- liftBase $ readMVar var

  let
    a = r ^. unNodeStory . at listId . _Just
    q = mconcat $ take (a ^. a_version - p_version) (a ^. a_history)
    q_table = q ^. _PatchMap . at ngramsType . _Just

  pure (Versioned (a ^. a_version) q_table)




-- tableNgramsPut :: (HasInvalidError err, RepoCmdM env err m)
-- Apply the given patch to the DB and returns the patch to be applied on the
-- client.
-- TODO-ACCESS check
tableNgramsPut :: ( HasNodeStory env err m
                   , HasInvalidError err
                  , HasSettings env
                  )
                 => TabType
                 -> ListId
                 -> Versioned NgramsTablePatch
                 -> m (Versioned NgramsTablePatch)
tableNgramsPut tabType listId (Versioned p_version p_table)
  | p_table == mempty = do
      printDebug "[tableNgramsPut]" ("TableEmpty" :: Text)
      let ngramsType        = ngramsTypeFromTabType tabType
      tableNgramsPull listId ngramsType p_version

  | otherwise         = do
      printDebug "[tableNgramsPut]" ("TableNonEmpty" :: Text)
      let ngramsType        = ngramsTypeFromTabType tabType
          (p, p_validity)   = PM.singleton ngramsType p_table

      assertValid p_validity

      ret <- commitStatePatch listId (Versioned p_version p)
        <&> v_data %~ (view (_PatchMap . at ngramsType . _Just))

      pure ret



tableNgramsPostChartsAsync :: ( HasNodeStory env err m
                              , FlowCmdM     env err m
                              , HasNodeError err
                              , HasSettings env
                              )
                            => UpdateTableNgramsCharts
                            -> (JobLog -> m ())
                            -> m JobLog
tableNgramsPostChartsAsync utn logStatus = do
      let tabType = utn ^. utn_tab_type
      let listId = utn ^. utn_list_id

      node <- getNode listId
      let nId = node ^. node_id
          _uId = node ^. node_user_id
          mCId = node ^. node_parent_id

      -- printDebug "[tableNgramsPostChartsAsync] tabType" tabType
      -- printDebug "[tableNgramsPostChartsAsync] listId" listId

      case mCId of
        Nothing -> do
          printDebug "[tableNgramsPostChartsAsync] can't update charts, no parent, nId" nId
          pure $ jobLogFail $ jobLogInit 1
        Just cId -> do
          case tabType of
            Authors -> do
              -- printDebug "[tableNgramsPostChartsAsync] Authors, updating Pie, cId" cId
              (logRef, logRefSuccess, getRef) <- runJobLog 1 logStatus
              logRef
              _ <- Metrics.updatePie cId (Just listId) tabType Nothing
              logRefSuccess

              getRef
            Institutes -> do
              -- printDebug "[tableNgramsPostChartsAsync] Institutes, updating Tree, cId" cId
              -- printDebug "[tableNgramsPostChartsAsync] updating tree StopTerm, cId" cId
              (logRef, logRefSuccess, getRef) <- runJobLog 3 logStatus
              logRef
              _ <- Metrics.updateTree cId (Just listId) tabType StopTerm
              -- printDebug "[tableNgramsPostChartsAsync] updating tree CandidateTerm, cId" cId
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType CandidateTerm
              -- printDebug "[tableNgramsPostChartsAsync] updating tree MapTerm, cId" cId
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType MapTerm
              logRefSuccess

              getRef
            Sources -> do
              -- printDebug "[tableNgramsPostChartsAsync] Sources, updating chart, cId" cId
              (logRef, logRefSuccess, getRef) <- runJobLog 1 logStatus
              logRef
              _ <- Metrics.updatePie cId (Just listId) tabType Nothing
              logRefSuccess

              getRef
            Terms -> do
              -- printDebug "[tableNgramsPostChartsAsync] Terms, updating Metrics (Histo), cId" cId
              (logRef, logRefSuccess, getRef) <- runJobLog 6 logStatus
              logRef
{-
              _ <- Metrics.updateChart cId (Just listId) tabType Nothing
              logRefSuccess
              _ <- Metrics.updatePie cId (Just listId) tabType Nothing
              logRefSuccess
              _ <- Metrics.updateScatter cId (Just listId) tabType Nothing
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType StopTerm
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType CandidateTerm
              logRefSuccess
              _ <- Metrics.updateTree cId (Just listId) tabType MapTerm
-}
              logRefSuccess

              getRef
            _ -> do
              printDebug "[tableNgramsPostChartsAsync] no update for tabType = " tabType
              pure $ jobLogFail $ jobLogInit 1

  {-
  { _ne_list        :: ListType
  If we merge the parents/children we can potentially create cycles!
  , _ne_parent      :: Maybe NgramsTerm
  , _ne_children    :: MSet NgramsTerm
  }
  -}

getNgramsTableMap :: HasNodeStory env err m
                  => NodeId
                  -> TableNgrams.NgramsType
                  -> m (Versioned NgramsTableMap)
getNgramsTableMap nodeId ngramsType = do
  v    <- getNodeStoryVar [nodeId]
  repo <- liftBase $ readMVar v
  pure $ Versioned (repo ^. unNodeStory . at nodeId . _Just . a_version)
                   (repo ^. unNodeStory . at nodeId . _Just . a_state . at ngramsType . _Just)


dumpJsonTableMap :: HasNodeStory env err m
                 => Text
                 -> NodeId
                 -> TableNgrams.NgramsType
                 -> m ()
dumpJsonTableMap fpath nodeId ngramsType = do
  m <- getNgramsTableMap nodeId ngramsType
  liftBase $ DTL.writeFile (unpack fpath) (DAT.encodeToLazyText m)
  pure ()


type MinSize = Int
type MaxSize = Int

-- | TODO Errors management
--  TODO: polymorphic for Annuaire or Corpus or ...
-- | Table of Ngrams is a ListNgrams formatted (sorted and/or cut).
-- TODO: should take only one ListId


getTableNgrams :: forall env err m.
                  (HasNodeStory env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => NodeType -> NodeId -> TabType
               -> ListId -> Limit -> Maybe Offset
               -> Maybe ListType
               -> Maybe MinSize -> Maybe MaxSize
               -> Maybe OrderBy
               -> (NgramsTerm -> Bool)
               -> m (VersionedWithCount NgramsTable)
getTableNgrams _nType nId tabType listId limit_ offset
               listType minSize maxSize orderBy searchQuery = do

  t0 <- getTime
  -- lIds <- selectNodesWithUsername NodeList userMaster
  let
    ngramsType = ngramsTypeFromTabType tabType
    offset'  = maybe 0 identity offset
    listType' = maybe (const True) (==) listType
    minSize'  = maybe (const True) (<=) minSize
    maxSize'  = maybe (const True) (>=) maxSize

    selected_node n = minSize'     s
                   && maxSize'     s
                   && searchQuery  (n ^. ne_ngrams)
                   && listType'    (n ^. ne_list)
      where
        s = n ^. ne_size

    selected_inner roots n = maybe False (`Set.member` roots) (n ^. ne_root)

    ---------------------------------------
    sortOnOrder Nothing = identity
    sortOnOrder (Just TermAsc)   = List.sortOn $ view ne_ngrams
    sortOnOrder (Just TermDesc)  = List.sortOn $ Down . view ne_ngrams
    sortOnOrder (Just ScoreAsc)  = List.sortOn $ view ne_occurrences
    sortOnOrder (Just ScoreDesc) = List.sortOn $ Down . view ne_occurrences

    ---------------------------------------

    filteredNodes :: Map NgramsTerm NgramsElement -> [NgramsElement]
    filteredNodes tableMap = rootOf <$> list & filter selected_node
      where
        rootOf ne = maybe ne (\r -> fromMaybe (panic "getTableNgrams: invalid root")
                                              (tableMap ^. at r)
                             )
                             (ne ^. ne_root)
        list = tableMap ^.. each

    ---------------------------------------
    selectAndPaginate :: Map NgramsTerm NgramsElement -> [NgramsElement]
    selectAndPaginate tableMap = roots <> inners
      where
        list = tableMap ^.. each
        rootOf ne = maybe ne (\r -> fromMaybe (panic "getTableNgrams: invalid root")
                                              (tableMap ^. at r)
                             )
                             (ne ^. ne_root)
        selected_nodes = list & take limit_
                              . drop offset'
                              . filter selected_node
                              . sortOnOrder orderBy
        roots = rootOf <$> selected_nodes
        rootsSet = Set.fromList (_ne_ngrams <$> roots)
        inners = list & filter (selected_inner rootsSet)

    ---------------------------------------
    setScores :: forall t. Each t t NgramsElement NgramsElement => Bool -> t -> m t
    setScores False table = pure table
    setScores True  table = do
      let ngrams_terms = table ^.. each . ne_ngrams
      t1 <- getTime
      occurrences <- getOccByNgramsOnlyFast' nId
                                             listId
                                            ngramsType
                                            ngrams_terms
      t2 <- getTime
      liftBase $ hprint stderr
        ("getTableNgrams/setScores #ngrams=" % int % " time=" % hasTime % "\n")
        (length ngrams_terms) t1 t2
      {-
      occurrences <- getOccByNgramsOnlySlow nType nId
                                            (lIds <> [listId])
                                            ngramsType
                                            ngrams_terms
      -}
      let
        setOcc ne = ne & ne_occurrences .~ sumOf (at (ne ^. ne_ngrams) . _Just) occurrences

      pure $ table & each %~ setOcc
    ---------------------------------------

  -- lists <- catMaybes <$> listsWith userMaster
  -- trace (show lists) $
  -- getNgramsTableMap ({-lists <>-} listIds) ngramsType


  let scoresNeeded = needsScores orderBy
  tableMap1 <- getNgramsTableMap listId ngramsType
  t1 <- getTime
  tableMap2 <- tableMap1 & v_data %%~ setScores scoresNeeded
                                    . Map.mapWithKey ngramsElementFromRepo

  fltr <- tableMap2 & v_data %%~ fmap NgramsTable . setScores (not scoresNeeded)
                                                  . filteredNodes
  let fltrCount = length $ fltr ^. v_data . _NgramsTable

  t2 <- getTime
  tableMap3 <- tableMap2 & v_data %%~ fmap NgramsTable
                                    . setScores (not scoresNeeded)
                                    . selectAndPaginate
  t3 <- getTime
  liftBase $ hprint stderr
            ("getTableNgrams total=" % hasTime
                          % " map1=" % hasTime
                          % " map2=" % hasTime
                          % " map3=" % hasTime
                          % " sql="  % (if scoresNeeded then "map2" else "map3")
                          % "\n"
            ) t0 t3 t0 t1 t1 t2 t2 t3
  pure $ toVersionedWithCount fltrCount tableMap3



scoresRecomputeTableNgrams :: forall env err m.
  (HasNodeStory env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
  => NodeId -> TabType -> ListId -> m Int
scoresRecomputeTableNgrams nId tabType listId = do
  tableMap <- getNgramsTableMap listId ngramsType
  _ <- tableMap & v_data %%~ setScores
                           . Map.mapWithKey ngramsElementFromRepo

  pure $ 1
  where
    ngramsType = ngramsTypeFromTabType tabType

    setScores :: forall t. Each t t NgramsElement NgramsElement => t -> m t
    setScores table = do
      let ngrams_terms = table ^.. each . ne_ngrams
      occurrences <- getOccByNgramsOnlyFast' nId
                                             listId
                                            ngramsType
                                            ngrams_terms
      let
        setOcc ne = ne & ne_occurrences .~ sumOf (at (ne ^. ne_ngrams) . _Just) occurrences

      pure $ table & each %~ setOcc




-- APIs

-- TODO: find a better place for the code above, All APIs stay here

data OrderBy = TermAsc | TermDesc | ScoreAsc | ScoreDesc
             deriving (Generic, Enum, Bounded, Read, Show)

instance FromHttpApiData OrderBy
  where
    parseUrlPiece "TermAsc"   = pure TermAsc
    parseUrlPiece "TermDesc"  = pure TermDesc
    parseUrlPiece "ScoreAsc"  = pure ScoreAsc
    parseUrlPiece "ScoreDesc" = pure ScoreDesc
    parseUrlPiece _           = Left "Unexpected value of OrderBy"


instance ToParamSchema OrderBy
instance FromJSON  OrderBy
instance ToJSON    OrderBy
instance ToSchema  OrderBy
instance Arbitrary OrderBy
  where
    arbitrary = elements [minBound..maxBound]

needsScores :: Maybe OrderBy -> Bool
needsScores (Just ScoreAsc)  = True
needsScores (Just ScoreDesc) = True
needsScores _ = False

type TableNgramsApiGet = Summary " Table Ngrams API Get"
                      :> QueryParamR "ngramsType"  TabType
                      :> QueryParamR "list"        ListId
                      :> QueryParamR "limit"       Limit
                      :> QueryParam  "offset"      Offset
                      :> QueryParam  "listType"    ListType
                      :> QueryParam  "minTermSize" MinSize
                      :> QueryParam  "maxTermSize" MaxSize
                      :> QueryParam  "orderBy"     OrderBy
                      :> QueryParam  "search"      Text
                      :> Get    '[JSON] (VersionedWithCount NgramsTable)

type TableNgramsApiPut = Summary " Table Ngrams API Change"
                       :> QueryParamR "ngramsType" TabType
                       :> QueryParamR "list"       ListId
                       :> ReqBody '[JSON] (Versioned NgramsTablePatch)
                       :> Put     '[JSON] (Versioned NgramsTablePatch)

type RecomputeScoresNgramsApiGet = Summary " Recompute scores for ngrams table"
                       :> QueryParamR "ngramsType"  TabType
                       :> QueryParamR "list"        ListId
                       :> "recompute" :> Post '[JSON] Int

type TableNgramsApiGetVersion = Summary " Table Ngrams API Get Version"
                      :> QueryParamR "ngramsType"  TabType
                      :> QueryParamR "list"        ListId
                      :> Get    '[JSON] Version

type TableNgramsApi =  TableNgramsApiGet
                  :<|> TableNgramsApiPut
                  :<|> RecomputeScoresNgramsApiGet
                  :<|> "version" :> TableNgramsApiGetVersion
                  :<|> TableNgramsAsyncApi

type TableNgramsAsyncApi = Summary "Table Ngrams Async API"
                           :> "async"
                           :> "charts"
                           :> "update"
                           :> AsyncJobs JobLog '[JSON] UpdateTableNgramsCharts JobLog

getTableNgramsCorpus :: (HasNodeStory env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => NodeId
               -> TabType
               -> ListId
               -> Limit
               -> Maybe Offset
               -> Maybe ListType
               -> Maybe MinSize -> Maybe MaxSize
               -> Maybe OrderBy
               -> Maybe Text -- full text search
               -> m (VersionedWithCount NgramsTable)
getTableNgramsCorpus nId tabType listId limit_ offset listType minSize maxSize orderBy mt =
  getTableNgrams NodeCorpus nId tabType listId limit_ offset listType minSize maxSize orderBy searchQuery
    where
      searchQuery (NgramsTerm nt) = maybe (const True) isInfixOf mt nt



getTableNgramsVersion :: (HasNodeStory env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => NodeId
               -> TabType
               -> ListId
               -> m Version
getTableNgramsVersion _nId _tabType listId = currentVersion listId



  -- TODO: limit?
  -- Versioned { _v_version = v } <- getTableNgramsCorpus nId tabType listId 100000 Nothing Nothing Nothing Nothing Nothing Nothing
  -- This line above looks like a waste of computation to finally get only the version.
  -- See the comment about listNgramsChangedSince.


-- | Text search is deactivated for now for ngrams by doc only
getTableNgramsDoc :: (HasNodeStory env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => DocId -> TabType
               -> ListId -> Limit -> Maybe Offset
               -> Maybe ListType
               -> Maybe MinSize -> Maybe MaxSize
               -> Maybe OrderBy
               -> Maybe Text -- full text search
               -> m (VersionedWithCount NgramsTable)
getTableNgramsDoc dId tabType listId limit_ offset listType minSize maxSize orderBy _mt = do
  ns <- selectNodesWithUsername NodeList userMaster
  let ngramsType = ngramsTypeFromTabType tabType
  ngs <- selectNgramsByDoc (ns <> [listId]) dId ngramsType
  let searchQuery (NgramsTerm nt) = flip S.member (S.fromList ngs) nt
  getTableNgrams NodeDocument dId tabType listId limit_ offset listType minSize maxSize orderBy searchQuery



apiNgramsTableCorpus :: ( GargServerC env err m
                        )
                     => NodeId -> ServerT TableNgramsApi m
apiNgramsTableCorpus cId =  getTableNgramsCorpus       cId
                       :<|> tableNgramsPut
                       :<|> scoresRecomputeTableNgrams cId
                       :<|> getTableNgramsVersion      cId
                       :<|> apiNgramsAsync             cId

apiNgramsTableDoc :: ( GargServerC env err m
                     )
                  => DocId -> ServerT TableNgramsApi m
apiNgramsTableDoc dId =  getTableNgramsDoc          dId
                    :<|> tableNgramsPut
                    :<|> scoresRecomputeTableNgrams dId
                    :<|> getTableNgramsVersion      dId
                    :<|> apiNgramsAsync             dId

apiNgramsAsync :: NodeId -> GargServer TableNgramsAsyncApi
apiNgramsAsync _dId =
  serveJobsAPI $
    JobFunction $ \i log ->
      let
        log' x = do
          printDebug "tableNgramsPostChartsAsync" x
          liftBase $ log x
      in tableNgramsPostChartsAsync i log'

-- Did the given list of ngrams changed since the given version?
-- The returned value is versioned boolean value, meaning that one always retrieve the
-- latest version.
-- If the given version is negative then one simply receive the latest version and True.
-- Using this function is more precise than simply comparing the latest version number
-- with the local version number. Indeed there might be no change to this particular list
-- and still the version number has changed because of other lists.
--
-- Here the added value is to make a compromise between precision, computation, and bandwidth:
-- * currentVersion: good computation, good bandwidth, bad precision.
-- * listNgramsChangedSince: good precision, good bandwidth, bad computation.
-- * tableNgramsPull: good precision, good bandwidth (if you use the received data!), bad computation.
listNgramsChangedSince :: HasNodeStory env err m
                       => ListId -> TableNgrams.NgramsType -> Version -> m (Versioned Bool)
listNgramsChangedSince listId ngramsType version
  | version < 0 =
      Versioned <$> currentVersion listId <*> pure True
  | otherwise   =
      tableNgramsPull listId ngramsType version & mapped . v_data %~ (== mempty)


