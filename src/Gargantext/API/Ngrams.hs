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

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans #-}

module Gargantext.API.Ngrams
  where

import Prelude (Enum, Bounded, Semigroup(..), minBound, maxBound, round)
-- import Gargantext.Database.Schema.User  (UserId)
import Data.Functor (($>))
import Data.Patch.Class (Replace, replace, Action(act), Applicable(..),
                         Composable(..), Group(..), Transformable(..),
                         PairPatch(..), Patched, ConflictResolution,
                         ConflictResolutionReplace,
                         SimpleConflictResolution')
import qualified Data.Map.Strict.Patch as PM
import Data.Monoid
--import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Data.Tuple.Extra (first)
-- import qualified Data.Map.Strict as DM
import Data.Map.Strict (Map, mapKeys, fromListWith)
--import qualified Data.Set as Set
import Control.Concurrent
import Control.Lens (makeLenses, makePrisms, Getter, Prism', prism', Iso', iso, (^..), (.~), (#), to, {-withIndex, folded, ifolded,-} view, (^.), (+~), (%~), at, _Just, Each(..), dropping, taking)
import Control.Monad (guard)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Either(Either(Left))
import Data.Map (lookup)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Swagger hiding (version, patch)
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Schema.Node (defaultList, HasNodeError)
-- import Gargantext.Database.Schema.Ngrams (NgramsTypeId, ngramsTypeId)
import Gargantext.Database.Schema.Ngrams (NgramsType, NgramsTableData(..))
import qualified Gargantext.Database.Schema.Ngrams as Ngrams
-- import Gargantext.Database.Schema.NodeNgram hiding (Action)
import Gargantext.Database.Utils (CmdM)
import Gargantext.Prelude
-- import Gargantext.Core.Types (ListTypeId, listTypeId)
import Gargantext.Core.Types (ListType(..), ListId, CorpusId, Limit, Offset)
import Servant hiding (Patch)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------
--data FacetFormat = Table | Chart
data TabType   = Docs     | Terms  | Sources | Authors | Institutes | Trash
               | Contacts
  deriving (Generic, Enum, Bounded)

instance FromHttpApiData TabType
  where
    parseUrlPiece "Docs"       = pure Docs
    parseUrlPiece "Terms"      = pure Terms
    parseUrlPiece "Sources"    = pure Sources
    parseUrlPiece "Institutes" = pure Institutes
    parseUrlPiece "Authors"    = pure Authors
    parseUrlPiece "Trash"      = pure Trash
    
    parseUrlPiece "Contacts"   = pure Contacts
    
    parseUrlPiece _            = Left "Unexpected value of TabType"

instance ToParamSchema   TabType
instance ToJSON    TabType
instance FromJSON  TabType
instance ToSchema  TabType
instance Arbitrary TabType
  where
    arbitrary = elements [minBound .. maxBound]

------------------------------------------------------------------------
type NgramsTerm = Text

data NgramsElement =
     NgramsElement { _ne_ngrams      :: NgramsTerm
                   , _ne_list        :: ListType
                   , _ne_occurrences :: Int
                   , _ne_parent      :: Maybe NgramsTerm
                   , _ne_children    :: Set NgramsTerm
                   }
      deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_ne_") ''NgramsElement
makeLenses ''NgramsElement

instance ToSchema NgramsElement
instance Arbitrary NgramsElement where
  arbitrary = elements [NgramsElement "sport" GraphList 1 Nothing mempty]

------------------------------------------------------------------------
newtype NgramsTable = NgramsTable [NgramsElement]
  deriving (Ord, Eq, Generic, ToJSON, FromJSON, Show)

makePrisms ''NgramsTable

instance Each NgramsTable NgramsTable NgramsElement NgramsElement where
  each = _NgramsTable . each

-- TODO discuss
-- | TODO Check N and Weight
toNgramsElement :: [NgramsTableData] -> [NgramsElement]
toNgramsElement ns = map toNgramsElement' ns
    where
      toNgramsElement' (NgramsTableData _ p t _ lt w) = NgramsElement t lt' (round w) p' c'
        where
          p' = case p of
                 Nothing -> Nothing
                 Just x  -> lookup x mapParent
          c' = maybe mempty identity $ lookup t mapChildren
          lt' = maybe (panic "API.Ngrams: listypeId") identity lt
      
      mapParent :: Map Int Text
      mapParent   = fromListWith (<>) $ map (\(NgramsTableData i _ t _ _ _) -> (i,t)) ns
      
      mapChildren :: Map Text (Set Text)
      mapChildren = mapKeys (\i -> (maybe (panic "API.Ngrams.mapChildren: ParentId with no Terms: Impossible") identity $ lookup i mapParent))
                  $ fromListWith (<>)
                  $ map (first fromJust)
                  $ filter (isJust . fst)
                  $ map (\(NgramsTableData _ p t _ _ _) -> (p, Set.singleton t)) ns


instance Arbitrary NgramsTable where
  arbitrary = elements
              [ NgramsTable
                [ NgramsElement "animal"  GraphList     1  Nothing       (Set.fromList ["dog", "cat"])
                , NgramsElement "cat"     GraphList     1 (Just "animal") mempty
                , NgramsElement "cats"    StopList      4  Nothing        mempty
                , NgramsElement "dog"     GraphList     3 (Just "animal")(Set.fromList ["dogs"])
                , NgramsElement "dogs"    StopList      4 (Just "dog")    mempty
                , NgramsElement "fox"     GraphList     1  Nothing        mempty
                , NgramsElement "object"  CandidateList 2  Nothing        mempty
                , NgramsElement "nothing" StopList      4  Nothing        mempty
                , NgramsElement "organic" GraphList     3  Nothing        (Set.singleton "flower")
                , NgramsElement "flower"  GraphList     3 (Just "organic") mempty
                , NgramsElement "moon"    CandidateList 1  Nothing         mempty
                , NgramsElement "sky"     StopList      1  Nothing         mempty
                ]
              ]
instance ToSchema NgramsTable

------------------------------------------------------------------------
type NgramsTableMap = Map NgramsTerm NgramsElement

------------------------------------------------------------------------
-- On the Client side:
--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data PatchSet a = PatchSet
  { _rem :: Set a
  , _add :: Set a
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''PatchSet

instance (Ord a, Arbitrary a) => Arbitrary (PatchSet a) where
  arbitrary = PatchSet <$> arbitrary <*> arbitrary

type instance Patched (PatchSet a) = Set a

type ConflictResolutionPatchSet a = SimpleConflictResolution' (Set a)
type instance ConflictResolution (PatchSet a) = ConflictResolutionPatchSet a

instance Ord a => Semigroup (PatchSet a) where
  p <> q = PatchSet { _rem = (q ^. rem) `Set.difference` (p ^. add) <> p ^. rem
                    , _add = (q ^. add) `Set.difference` (p ^. rem) <> p ^. add
                    } -- TODO Review

instance Ord a => Monoid (PatchSet a) where
  mempty = PatchSet mempty mempty

instance Ord a => Group (PatchSet a) where
  invert (PatchSet r a) = PatchSet a r

instance Ord a => Composable (PatchSet a) where
  composable _ _ = mempty

instance Ord a => Action (PatchSet a) (Set a) where
  act p source = (source `Set.difference` (p ^. rem)) <> p ^. add

instance Applicable (PatchSet a) (Set a) where
  applicable _ _ = mempty

instance Ord a => Validity (PatchSet a) where
  validate p = check (Set.disjoint (p ^. rem) (p ^. add)) "_rem and _add should be dijoint"

instance Ord a => Transformable (PatchSet a) where
  transformable = undefined

  conflicts _p _q = undefined

  transformWith conflict p q = undefined conflict p q

instance ToJSON a => ToJSON (PatchSet a) where
  toJSON     = genericToJSON     $ unPrefix "_"
  toEncoding = genericToEncoding $ unPrefix "_"

instance (Ord a, FromJSON a) => FromJSON (PatchSet a) where
  parseJSON = genericParseJSON $ unPrefix "_"

instance ToSchema a => ToSchema (PatchSet a)

instance ToSchema a => ToSchema (Replace a) where
  declareNamedSchema (_ :: proxy (Replace a)) = do
    -- TODO Keep constructor is not supported here.
    aSchema <- declareSchemaRef (Proxy :: Proxy a)
    return $ NamedSchema (Just "Replace") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          InsOrdHashMap.fromList
          [ ("old", aSchema)
          , ("new", aSchema)
          ]
      & required .~ [ "old", "new" ]

data NgramsPatch =
     NgramsPatch { _patch_children :: PatchSet NgramsTerm
                 , _patch_list     :: Replace ListType   -- TODO Map UserId ListType
                 }
      deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_") ''NgramsPatch
makeLenses ''NgramsPatch

instance ToSchema  NgramsPatch

instance Arbitrary NgramsPatch where
  arbitrary = NgramsPatch <$> arbitrary <*> (replace <$> arbitrary <*> arbitrary)

type NgramsPatchIso = PairPatch (PatchSet NgramsTerm) (Replace ListType)

_NgramsPatch :: Iso' NgramsPatch NgramsPatchIso
_NgramsPatch = iso (\(NgramsPatch c l) -> c :*: l) (\(c :*: l) -> NgramsPatch c l)

instance Semigroup NgramsPatch where
  p <> q = _NgramsPatch # (p ^. _NgramsPatch <> q ^. _NgramsPatch)

instance Monoid NgramsPatch where
  mempty = _NgramsPatch # mempty

instance Validity NgramsPatch where
  validate p = p ^. _NgramsPatch . to validate

instance Transformable NgramsPatch where
  transformable p q = transformable (p ^. _NgramsPatch) (q ^. _NgramsPatch)

  conflicts p q = conflicts (p ^. _NgramsPatch) (q ^. _NgramsPatch)

  transformWith conflict p q = (_NgramsPatch # p', _NgramsPatch # q')
    where
      (p', q') = transformWith conflict (p ^. _NgramsPatch) (q ^. _NgramsPatch)

type ConflictResolutionNgramsPatch =
  ( ConflictResolutionPatchSet NgramsTerm
  , ConflictResolutionReplace ListType
  )
type instance ConflictResolution NgramsPatch =
  ConflictResolutionNgramsPatch

type PatchedNgramsPatch = (Set NgramsTerm, ListType)
  -- ~ Patched NgramsPatchIso
type instance Patched NgramsPatch = PatchedNgramsPatch

instance Applicable NgramsPatch (Maybe NgramsElement) where
  applicable p Nothing   = check (p == mempty) "NgramsPatch should be empty here"
  applicable p (Just ne) =
    -- TODO how to patch _ne_parent ?
    applicable (p ^. patch_children) (ne ^. ne_children) <>
    applicable (p ^. patch_list)     (ne ^. ne_list)

instance Action NgramsPatch (Maybe NgramsElement) where
  act _ Nothing   = Nothing
  act p (Just ne) =
    -- TODO how to patch _ne_parent ?
    ne & ne_children %~ act (p ^. patch_children)
       & ne_list     %~ act (p ^. patch_list)
       & Just

type PatchMap = PM.PatchMap

newtype NgramsTablePatch = NgramsTablePatch (PatchMap NgramsTerm NgramsPatch)
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Semigroup, Monoid, Validity, Transformable)

--instance (Ord k, Action pv (Maybe v)) => Action (PatchMap k pv) (Map k v) where
--
type instance ConflictResolution NgramsTablePatch =
  NgramsTerm -> ConflictResolutionNgramsPatch

type PatchedNgramsTablePatch = Map NgramsTerm PatchedNgramsPatch
  -- ~ Patched (PatchMap NgramsTerm NgramsPatch)
type instance Patched NgramsTablePatch = PatchedNgramsTablePatch

makePrisms ''NgramsTablePatch
instance ToSchema  (PatchMap NgramsTerm NgramsPatch)
instance ToSchema  NgramsTablePatch

instance Applicable NgramsTablePatch (Maybe NgramsTableMap) where
  applicable p = applicable (p ^. _NgramsTablePatch)

instance Action NgramsTablePatch (Maybe NgramsTableMap) where
  act p = act (p ^. _NgramsTablePatch)
              -- (v ^? _Just . _NgramsTable)
              -- ^? _Just . from _NgramsTable

instance Arbitrary NgramsTablePatch where
  arbitrary = NgramsTablePatch <$> PM.fromMap <$> arbitrary

-- Should it be less than an Lens' to preserve PatchMap's abstraction.
-- ntp_ngrams_patches :: Lens' NgramsTablePatch (Map NgramsTerm NgramsPatch)
-- ntp_ngrams_patches = _NgramsTablePatch .  undefined

-- TODO: replace by mempty once we have the Monoid instance
emptyNgramsTablePatch :: NgramsTablePatch
emptyNgramsTablePatch = NgramsTablePatch mempty

------------------------------------------------------------------------
------------------------------------------------------------------------
type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }
  deriving (Generic)
deriveJSON (unPrefix "_v_") ''Versioned
makeLenses ''Versioned
instance ToSchema a => ToSchema (Versioned a)
instance Arbitrary a => Arbitrary (Versioned a) where
  arbitrary = Versioned 1 <$> arbitrary -- TODO 1 is constant so far

{-
-- TODO sequencs of modifications (Patchs)
type NgramsIdPatch = Patch NgramsId NgramsPatch

ngramsPatch :: Int -> NgramsPatch
ngramsPatch n = NgramsPatch (DM.fromList [(1, StopList)]) (Set.fromList [n]) Set.empty

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

type TableNgramsApiGet = Summary " Table Ngrams API Get"
                      :> QueryParam "ngramsType"   TabType
                      :> QueryParam "list"   ListId
                      :> QueryParam "limit"  Limit
                      :> QueryParam "offset" Offset
                      :> Get    '[JSON] (Versioned NgramsTable)

type TableNgramsApi = Summary " Table Ngrams API Change"
                      :> QueryParam "ngramsType"   TabType
                      :> QueryParam "list"   ListId
                      :> ReqBody '[JSON] (Versioned NgramsTablePatch)
                      :> Put     '[JSON] (Versioned NgramsTablePatch)

data NgramError = UnsupportedVersion
  deriving (Show)

class HasNgramError e where
  _NgramError :: Prism' e NgramError

instance HasNgramError ServantErr where
  _NgramError = prism' make match
    where
      err = err500 { errBody = "NgramError: Unsupported version" }
      make UnsupportedVersion = err
      match e = guard (e == err) $> UnsupportedVersion

ngramError :: (MonadError e m, HasNgramError e) => NgramError -> m a
ngramError nne = throwError $ _NgramError # nne

{-
-- TODO: Replace.old is ignored which means that if the current list
-- `GraphList` and that the patch is `Replace CandidateList StopList` then
-- the list is going to be `StopList` while it should keep `GraphList`.
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

ngramsTypeFromTabType :: Maybe TabType -> NgramsType
ngramsTypeFromTabType maybeTabType =
  let lieu = "Garg.API.Ngrams: " :: Text in
    case maybeTabType of
          Nothing  -> panic (lieu <> "Indicate the Table")
          Just tab -> case tab of
              Sources    -> Ngrams.Sources
              Authors    -> Ngrams.Authors
              Institutes -> Ngrams.Institutes
              Terms      -> Ngrams.NgramsTerms
              _          -> panic $ lieu <> "No Ngrams for this tab"

------------------------------------------------------------------------
data Repo s p = Repo
  { _r_version :: Version
  , _r_state   :: s
  , _r_history :: [p]
    -- ^ first patch in the list is the most recent
  }

makeLenses ''Repo

initRepo :: Monoid s => Repo s p
initRepo = Repo 1 mempty []

type NgramsState      = Map ListId (Map NgramsType NgramsTableMap)
type NgramsStatePatch = PatchMap ListId (PatchMap NgramsType NgramsTablePatch)
type NgramsRepo       = Repo NgramsState NgramsStatePatch

class HasRepoVar env where
  repoVar :: Getter env (MVar NgramsRepo)

instance HasRepoVar (MVar NgramsRepo) where
  repoVar = identity

type RepoCmdM env err m =
  ( CmdM env err m
  , HasRepoVar env
  , HasNodeError err
  )
------------------------------------------------------------------------

listTypeConflictResolution :: ListType -> ListType -> ListType
listTypeConflictResolution _ _ = undefined -- TODO Use Map User ListType

ngramsStatePatchConflictResolution
  :: ListId -> NgramsType -> NgramsTerm
  -> ConflictResolutionNgramsPatch
ngramsStatePatchConflictResolution _listId _ngramsType _ngramsTerm
  = ((<>) {- TODO think this through -}, listTypeConflictResolution)

makePrisms ''PM.PatchMap

class HasInvalidError e where
  _InvalidError :: Prism' e Validation

instance HasInvalidError ServantErr where
  _InvalidError = undefined {-prism' make match
    where
      err = err500 { errBody = "InvalidError" }
      make _ = err
      match e = guard (e == err) $> UnsupportedVersion-}

assertValid :: (MonadError e m, HasInvalidError e) => Validation -> m ()
assertValid v = when (not $ validationIsValid v) $ throwError $ _InvalidError # v

-- Apply the given patch to the DB and returns the patch to be applied on the
-- cilent.
-- TODO:
-- In this perliminary version the OT aspect is missing, therefore the version
-- number is always 1 and the returned patch is always empty.
tableNgramsPatch :: (HasNgramError err, HasNodeError err, HasInvalidError err,
                     RepoCmdM env err m)
                 => CorpusId -> Maybe TabType -> Maybe ListId
                 -> Versioned NgramsTablePatch
                 -> m (Versioned NgramsTablePatch)
tableNgramsPatch corpusId maybeTabType maybeList (Versioned p_version p_table) = do
  let ngramsType = ngramsTypeFromTabType maybeTabType
  listId <- maybe (defaultList corpusId) pure maybeList
  let (p0, p0_validity) = PM.singleton ngramsType p_table
  let (p, p_validity) = PM.singleton listId p0

  assertValid p0_validity
  assertValid p_validity

  var <- view repoVar
  (p'_applicable, vq') <- liftIO $ modifyMVar var $ \r ->
    let
      q = mconcat $ take (r ^. r_version - p_version) (r ^. r_history)
      (p', q') = transformWith ngramsStatePatchConflictResolution p q
      r' = r & r_version +~ 1
             & r_state   %~ act p'
             & r_history %~ (p' :)
      q'_table = q' ^. _PatchMap . at listId . _Just . _PatchMap . at ngramsType . _Just
      p'_applicable = applicable p' (r ^. r_state)
    in
    pure (r', (p'_applicable, Versioned (r' ^. r_version) q'_table))
  assertValid p'_applicable
  pure vq'

  {- DB version
  when (version /= 1) $ ngramError UnsupportedVersion
  updateNodeNgrams $ NodeNgramsUpdate
    { _nnu_user_list_id = listId
    , _nnu_lists_update = mkListsUpdate ngramsType patch
    , _nnu_rem_children = mkChildrenGroups _rem ngramsType patch
    , _nnu_add_children = mkChildrenGroups _add ngramsType patch
    }
  pure $ Versioned 1 emptyNgramsTablePatch
  -}

-- | TODO Errors management
--  TODO: polymorphic for Annuaire or Corpus or ...
getTableNgrams :: RepoCmdM env err m
               => CorpusId -> Maybe TabType
               -> Maybe ListId -> Maybe Limit -> Maybe Offset
               -- -> Maybe MinSize -> Maybe MaxSize
               -- -> Maybe ListType
               -- -> Maybe Text -- full text search
               -> m (Versioned NgramsTable)
getTableNgrams cId maybeTabType maybeListId mlimit moffset = do
  let ngramsType = ngramsTypeFromTabType maybeTabType
  listId <- maybe (defaultList cId) pure maybeListId

  let
    defaultLimit = 10 -- TODO
    limit_  = maybe defaultLimit identity mlimit
    offset_ = maybe 0 identity moffset

  v <- view repoVar
  repo <- liftIO $ readMVar v

  let ngrams = repo ^.. r_state
                      . at listId . _Just
                      . at ngramsType . _Just
                      . taking limit_ (dropping offset_ each)

  pure $ Versioned (repo ^. r_version) (NgramsTable ngrams)

  {-
  ngramsTableDatas <-
    Ngrams.getNgramsTableDb NodeDocument ngramsType (Ngrams.NgramsTableParam listId cId) limit_ offset_

  -- printDebug "ngramsTableDatas" ngramsTableDatas

  pure $ Versioned 1 $ NgramsTable (toNgramsElement ngramsTableDatas)
  -}
