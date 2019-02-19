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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans #-}

module Gargantext.API.Ngrams
  where

-- import Debug.Trace (trace)
import Prelude (Enum, Bounded, Semigroup(..), minBound, maxBound {-, round-}, error)
-- import Gargantext.Database.Schema.User  (UserId)
import Data.Functor (($>))
import Data.Patch.Class (Replace, replace, Action(act), Applicable(..),
                         Composable(..), Transformable(..),
                         PairPatch(..), Patched, ConflictResolution,
                         ConflictResolutionReplace, ours)
import qualified Data.Map.Strict.Patch as PM
import Data.Monoid
--import Data.Semigroup
import Data.Set (Set)
-- import qualified Data.List as List
-- import Data.Maybe (catMaybes)
-- import Data.Tuple.Extra (first)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
--import qualified Data.Set as Set
import Control.Category ((>>>))
import Control.Concurrent
import Control.Lens (makeLenses, makePrisms, Getter, Prism', prism', Iso', iso, from, (^..), (.~), (#), to, {-withIndex, folded, ifolded,-} view, (^.), (+~), (%~), at, _Just, Each(..), itraverse_, (.=), both, mapped)
import Control.Monad (guard)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Aeson.TH (deriveJSON)
import Data.Either(Either(Left))
-- import Data.Map (lookup)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Swagger hiding (version, patch)
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix)
-- import Gargantext.Database.Schema.Ngrams (NgramsTypeId, ngramsTypeId, NgramsTableData(..))
--import Gargantext.Database.Config (userMaster)
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Database.Utils (fromField', HasConnection)
--import Gargantext.Database.Lists (listsWith)
import Gargantext.Database.Schema.Node (HasNodeError)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Gargantext.Database.Schema.Ngrams as Ngrams
-- import Gargantext.Database.Schema.NodeNgram hiding (Action)
import Gargantext.Prelude
-- import Gargantext.Core.Types (ListTypeId, listTypeId)
import Gargantext.Core.Types (ListType(..), NodeId, ListId, CorpusId, Limit, Offset)
import Servant hiding (Patch)
import System.FileLock (FileLock)
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

newtype MSet a = MSet (Map a ())
  deriving (Eq, Ord, Show, Generic, Arbitrary, Semigroup, Monoid)

instance ToJSON a => ToJSON (MSet a) where
  toJSON     (MSet m) = toJSON     (Map.keys m)
  toEncoding (MSet m) = toEncoding (Map.keys m)

mSetFromSet :: Set a -> MSet a
mSetFromSet = MSet . Map.fromSet (const ())

mSetFromList :: Ord a => [a] -> MSet a
mSetFromList = MSet . Map.fromList . map (\x -> (x, ()))

instance (Ord a, FromJSON a) => FromJSON (MSet a) where
  parseJSON = fmap mSetFromList . parseJSON

instance (ToJSONKey a, ToSchema a) => ToSchema (MSet a) where
  -- TODO

------------------------------------------------------------------------
type NgramsTerm = Text

data NgramsElement =
     NgramsElement { _ne_ngrams      :: NgramsTerm
                   , _ne_list        :: ListType
                   , _ne_occurrences :: Int
                   , _ne_parent      :: Maybe NgramsTerm
                   , _ne_children    :: MSet NgramsTerm
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

type ListNgrams = NgramsTable

makePrisms ''NgramsTable

-- | Question: why these repetition of Type in this instance
-- may you document it please ?
instance Each NgramsTable NgramsTable NgramsElement NgramsElement where
  each = _NgramsTable . each

-- TODO discuss
-- | TODO Check N and Weight
{-
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
      mapParent   = Map.fromListWith (<>) $ map (\(NgramsTableData i _ t _ _ _) -> (i,t)) ns
      
      mapChildren :: Map Text (Set Text)
      mapChildren = Map.mapKeys (\i -> (maybe (panic "API.Ngrams.mapChildren: ParentId with no Terms: Impossible") identity $ lookup i mapParent))
                  $ Map.fromListWith (<>)
                  $ map (first fromJust)
                  $ filter (isJust . fst)
                  $ map (\(NgramsTableData _ p t _ _ _) -> (p, Set.singleton t)) ns
-}

mockTable :: NgramsTable
mockTable = NgramsTable
  [ NgramsElement "animal"  GraphList     1  Nothing       (mSetFromList ["dog", "cat"])
  , NgramsElement "cat"     GraphList     1 (Just "animal") mempty
  , NgramsElement "cats"    StopList      4  Nothing        mempty
  , NgramsElement "dog"     GraphList     3 (Just "animal")(mSetFromList ["dogs"])
  , NgramsElement "dogs"    StopList      4 (Just "dog")    mempty
  , NgramsElement "fox"     GraphList     1  Nothing        mempty
  , NgramsElement "object"  CandidateList 2  Nothing        mempty
  , NgramsElement "nothing" StopList      4  Nothing        mempty
  , NgramsElement "organic" GraphList     3  Nothing        (mSetFromList ["flower"])
  , NgramsElement "flower"  GraphList     3 (Just "organic") mempty
  , NgramsElement "moon"    CandidateList 1  Nothing         mempty
  , NgramsElement "sky"     StopList      1  Nothing         mempty
  ]

instance Arbitrary NgramsTable where
  arbitrary = pure mockTable

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
makePrisms ''PatchSet

instance ToJSON a => ToJSON (PatchSet a) where
  toJSON     = genericToJSON     $ unPrefix "_"
  toEncoding = genericToEncoding $ unPrefix "_"

instance (Ord a, FromJSON a) => FromJSON (PatchSet a) where
  parseJSON = genericParseJSON $ unPrefix "_"

{-
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
  composable _ _ = undefined

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

instance ToSchema a => ToSchema (PatchSet a)
-}

type AddRem = Replace (Maybe ())

remPatch, addPatch :: AddRem
remPatch = replace (Just ()) Nothing
addPatch = replace Nothing (Just ())

isRem :: Replace (Maybe ()) -> Bool
isRem = (== remPatch)

type PatchMap = PM.PatchMap

newtype PatchMSet a = PatchMSet (PatchMap a AddRem)
  deriving (Eq, Show, Generic, Validity, Semigroup, Monoid,
            Transformable, Composable)

type ConflictResolutionPatchMSet a = a -> ConflictResolutionReplace (Maybe ())
type instance ConflictResolution (PatchMSet a) = ConflictResolutionPatchMSet a

-- TODO this breaks module abstraction
makePrisms ''PM.PatchMap

makePrisms ''PatchMSet

_PatchMSetIso :: Ord a => Iso' (PatchMSet a) (PatchSet a)
_PatchMSetIso = _PatchMSet . _PatchMap . iso f g . from _PatchSet
  where
    f :: Ord a => Map a (Replace (Maybe ())) -> (Set a, Set a)
    f = Map.partition isRem >>> both %~ Map.keysSet

    g :: Ord a => (Set a, Set a) -> Map a (Replace (Maybe ()))
    g (rems, adds) = Map.fromSet (const remPatch) rems
                  <> Map.fromSet (const addPatch) adds

instance Ord a => Action (PatchMSet a) (MSet a) where
  act (PatchMSet p) (MSet m) = MSet $ act p m

instance Ord a => Applicable (PatchMSet a) (MSet a) where
  applicable (PatchMSet p) (MSet m) = applicable p m

instance (Ord a, ToJSON a) => ToJSON (PatchMSet a) where
  toJSON     = toJSON . view _PatchMSetIso
  toEncoding = toEncoding . view _PatchMSetIso

instance (Ord a, FromJSON a) => FromJSON (PatchMSet a) where
  parseJSON = fmap (_PatchMSetIso #) . parseJSON

instance (Ord a, Arbitrary a) => Arbitrary (PatchMSet a) where
  arbitrary = (PatchMSet . PM.fromMap) <$> arbitrary

instance ToSchema a => ToSchema (PatchMSet a) where
  -- TODO
  declareNamedSchema _ = undefined

type instance Patched (PatchMSet a) = MSet a

instance (Eq a, Arbitrary a) => Arbitrary (Replace a) where
  arbitrary = uncurry replace <$> arbitrary
    -- If they happen to be equal then the patch is Keep.

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
     NgramsPatch { _patch_children :: PatchMSet NgramsTerm
                 , _patch_list     :: Replace ListType   -- TODO Map UserId ListType
                 }
      deriving (Eq, Show, Generic)

deriveJSON (unPrefix "_") ''NgramsPatch
makeLenses ''NgramsPatch

instance ToSchema  NgramsPatch

instance Arbitrary NgramsPatch where
  arbitrary = NgramsPatch <$> arbitrary <*> (replace <$> arbitrary <*> arbitrary)

type NgramsPatchIso = PairPatch (PatchMSet NgramsTerm) (Replace ListType)

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
  ( ConflictResolutionPatchMSet NgramsTerm
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

instance Action NgramsPatch NgramsElement where
  act p = (ne_children %~ act (p ^. patch_children))
        . (ne_list     %~ act (p ^. patch_list))

instance Action NgramsPatch (Maybe NgramsElement) where
  act = fmap . act

newtype NgramsTablePatch = NgramsTablePatch (PatchMap NgramsTerm NgramsPatch)
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Semigroup, Monoid, Validity, Transformable)

instance FromField NgramsTablePatch
  where
    fromField = fromField'

instance FromField (PatchMap NgramsType (PatchMap NodeId NgramsTablePatch))
  where
    fromField = fromField'

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
  act p =
    fmap (execState (reParentNgramsTablePatch p)) .
    act (p ^. _NgramsTablePatch)

instance Arbitrary NgramsTablePatch where
  arbitrary = NgramsTablePatch <$> PM.fromMap <$> arbitrary

-- Should it be less than an Lens' to preserve PatchMap's abstraction.
-- ntp_ngrams_patches :: Lens' NgramsTablePatch (Map NgramsTerm NgramsPatch)
-- ntp_ngrams_patches = _NgramsTablePatch .  undefined

type ReParent a = forall m. MonadState NgramsTableMap m => a -> m ()

reParent :: Maybe NgramsTerm -> ReParent NgramsTerm
reParent parent child = at child . _Just . ne_parent .= parent

reParentAddRem :: NgramsTerm -> NgramsTerm -> ReParent AddRem
reParentAddRem parent child p =
  reParent (if isRem p then Nothing else Just parent) child

reParentNgramsPatch :: NgramsTerm -> ReParent NgramsPatch
reParentNgramsPatch parent ngramsPatch =
  itraverse_ (reParentAddRem parent) (ngramsPatch ^. patch_children . _PatchMSet . _PatchMap)
  -- TODO FoldableWithIndex/TraversableWithIndex for PatchMap

reParentNgramsTablePatch :: ReParent NgramsTablePatch
reParentNgramsTablePatch p = itraverse_ reParentNgramsPatch (p ^. _NgramsTablePatch. _PatchMap)
  -- TODO FoldableWithIndex/TraversableWithIndex for PatchMap

------------------------------------------------------------------------
------------------------------------------------------------------------
type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }
  deriving (Generic, Show)
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
                      :> QueryParams "list"   ListId
                      :> QueryParam "limit"  Limit
                      :> QueryParam "offset" Offset
                      :> Get    '[JSON] (Versioned NgramsTable)

type TableNgramsApi = Summary " Table Ngrams API Change"
                      :> QueryParam "ngramsType"   TabType
                      :> QueryParam' '[Required, Strict] "list" ListId
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
  deriving (Generic)

instance (FromJSON s, FromJSON p) => FromJSON (Repo s p) where
  parseJSON = genericParseJSON $ unPrefix "_r_"

instance (ToJSON s, ToJSON p) => ToJSON (Repo s p) where
  toJSON     = genericToJSON     $ unPrefix "_r_"
  toEncoding = genericToEncoding $ unPrefix "_r_"

makeLenses ''Repo

initRepo :: Monoid s => Repo s p
initRepo = Repo 1 mempty []

type NgramsRepo       = Repo NgramsState NgramsStatePatch
type NgramsState      = Map NgramsType (Map NodeId NgramsTableMap)
type NgramsStatePatch = PatchMap NgramsType (PatchMap NodeId NgramsTablePatch)

initMockRepo :: NgramsRepo
initMockRepo = Repo 1 s []
  where
    s = Map.singleton Ngrams.NgramsTerms
      $ Map.singleton 47254
      $ Map.fromList
      [ (n ^. ne_ngrams, n) | n <- mockTable ^. _NgramsTable ]

data RepoEnv = RepoEnv
  { _renv_var   :: !(MVar NgramsRepo)
  , _renv_saver :: !(IO ())
  , _renv_lock  :: !FileLock
  }
  deriving (Generic)

makeLenses ''RepoEnv

class HasRepoVar env where
  repoVar :: Getter env (MVar NgramsRepo)

instance HasRepoVar (MVar NgramsRepo) where
  repoVar = identity

class HasRepoSaver env where
  repoSaver :: Getter env (IO ())

class (HasRepoVar env, HasRepoSaver env) => HasRepo env where
  repoEnv :: Getter env RepoEnv

instance HasRepo RepoEnv where
  repoEnv = identity

instance HasRepoVar RepoEnv where
  repoVar = renv_var

instance HasRepoSaver RepoEnv where
  repoSaver = renv_saver

type RepoCmdM env err m =
  ( MonadReader env m
  , MonadError err m
  , MonadIO m
  , HasRepo env
  )
------------------------------------------------------------------------

saveRepo :: ( MonadReader env m, MonadIO m, HasRepoSaver env )
         => m ()
saveRepo = liftIO =<< view repoSaver

listTypeConflictResolution :: ListType -> ListType -> ListType
listTypeConflictResolution _ _ = undefined -- TODO Use Map User ListType

ngramsStatePatchConflictResolution
  :: NgramsType -> NodeId -> NgramsTerm
  -> ConflictResolutionNgramsPatch
ngramsStatePatchConflictResolution _ngramsType _nodeId _ngramsTerm
  = (const ours, ours)
  -- undefined {- TODO think this through -}, listTypeConflictResolution)

class HasInvalidError e where
  _InvalidError :: Prism' e Validation

instance HasInvalidError ServantErr where
  _InvalidError = panic "error" {-prism' make match
    where
      err = err500 { errBody = "InvalidError" }
      make _ = err
      match e = guard (e == err) $> UnsupportedVersion-}

-- assertValid :: (MonadError e m, HasInvalidError e) => Validation -> m ()
-- assertValid v = when (not $ validationIsValid v) $ throwError $ _InvalidError # v

assertValid :: MonadIO m => Validation -> m ()
assertValid v = when (not $ validationIsValid v) $ fail $ show v

-- Current state:
--   Insertions are not considered as patches,
--   they do not extend history,
--   they do not bump version.
insertNewOnly :: a -> Maybe b -> a
insertNewOnly m = maybe m (const $ error "insertNewOnly: impossible")
  -- TODO error handling

something :: Monoid a => Maybe a -> a
something Nothing  = mempty
something (Just a) = a

{- unused
-- TODO refactor with putListNgrams
copyListNgrams :: RepoCmdM env err m
               => NodeId -> NodeId -> NgramsType
               -> m ()
copyListNgrams srcListId dstListId ngramsType = do
  var <- view repoVar
  liftIO $ modifyMVar_ var $
    pure . (r_state . at ngramsType %~ (Just . f . something))
  saveRepo
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
  liftIO $ modifyMVar_ var $
    pure . (r_state . at ngramsType . _Just . at listId . _Just <>~ m)
  saveRepo
  where
    m = Map.fromList $ (\n -> (n ^. ne_ngrams, n)) <$> nes
-}

putListNgrams :: RepoCmdM env err m
              => NodeId -> NgramsType
              -> [NgramsElement] -> m ()
putListNgrams listId ngramsType nes = do
  var <- view repoVar
  liftIO $ modifyMVar_ var $
    pure . (r_state . at ngramsType %~ (Just . (at listId %~ (Just . (m <>) . something)) . something))
  saveRepo
  where
    m = Map.fromList $ (\n -> (n ^. ne_ngrams, n)) <$> nes

-- Apply the given patch to the DB and returns the patch to be applied on the
-- client.
-- TODO:
-- In this perliminary version the OT aspect is missing, therefore the version
-- number is always 1 and the returned patch is always empty.
tableNgramsPatch :: (HasNgramError err, HasInvalidError err,
                     RepoCmdM env err m)
                 => CorpusId -> Maybe TabType -> ListId
                 -> Versioned NgramsTablePatch
                 -> m (Versioned NgramsTablePatch)
tableNgramsPatch _corpusId maybeTabType listId (Versioned p_version p_table) = do
  let ngramsType        = ngramsTypeFromTabType maybeTabType
      (p0, p0_validity) = PM.singleton listId p_table
      (p, p_validity)   = PM.singleton ngramsType p0

  assertValid p0_validity
  assertValid p_validity

  var <- view repoVar
  vq' <- liftIO $ modifyMVar var $ \r -> do
    let
      q = mconcat $ take (r ^. r_version - p_version) (r ^. r_history)
      (p', q') = transformWith ngramsStatePatchConflictResolution p q
      r' = r & r_version +~ 1
             & r_state   %~ act p'
             & r_history %~ (p' :)
      q'_table = q' ^. _PatchMap . at ngramsType . _Just . _PatchMap . at listId . _Just
    assertValid $ transformable p q
    assertValid $ applicable p' (r ^. r_state)
    pure (r', Versioned (r' ^. r_version) q'_table)

  saveRepo
  pure vq'

  {- DB version
  when (version /= 1) $ ngramError UnsupportedVersion
  updateNodeNgrams $ NodeNgramsUpdate
    { _nnu_user_list_id = listId
    , _nnu_lists_update = mkListsUpdate ngramsType patch
    , _nnu_rem_children = mkChildrenGroups _rem ngramsType patch
    , _nnu_add_children = mkChildrenGroups _add ngramsType patch
    }
  pure $ Versioned 1 mempty
  -}

mergeNgramsElement :: NgramsElement -> NgramsElement -> NgramsElement
mergeNgramsElement _neOld neNew = neNew
  {-
  { _ne_list        :: ListType
  If we merge the parents/children we can potentially create cycles!
  , _ne_parent      :: Maybe NgramsTerm
  , _ne_children    :: MSet NgramsTerm
  }
  -}

getListNgrams :: RepoCmdM env err m
                => [NodeId] -> NgramsType -> m (Versioned ListNgrams)
getListNgrams nodeIds ngramsType = do
  v <- view repoVar
  repo <- liftIO $ readMVar v

  let
    ngramsMap = repo ^. r_state . at ngramsType . _Just

    ngrams =
      Map.unionsWith mergeNgramsElement
        [ ngramsMap ^. at nodeId . _Just | nodeId <- nodeIds ]

  pure $ Versioned (repo ^. r_version) (NgramsTable (ngrams ^.. each))


-- | TODO Errors management
--  TODO: polymorphic for Annuaire or Corpus or ...
-- | Table of Ngrams is a ListNgrams formatted (sorted and/or cut).
getTableNgrams :: (RepoCmdM env err m, HasNodeError err, HasConnection env)
               => CorpusId -> Maybe TabType
               -> [ListId] -> Maybe Limit -> Maybe Offset
               -- -> Maybe MinSize -> Maybe MaxSize
               -- -> Maybe ListType
               -- -> Maybe Text -- full text search
               -> m (Versioned NgramsTable)
getTableNgrams _cId maybeTabType listIds mlimit moffset = do
  let ngramsType = ngramsTypeFromTabType maybeTabType

  let
    defaultLimit = 10 -- TODO
    limit_  = maybe defaultLimit identity mlimit
    offset_ = maybe 0 identity moffset

  -- lists <- catMaybes <$> listsWith userMaster
  -- trace (show lists) $
  getListNgrams ({-lists <>-} listIds) ngramsType
    & mapped . v_data . _NgramsTable %~ (take limit_ . drop offset_)

