{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans #-}

module Gargantext.API.Ngrams
  ( TableNgramsApi
  , TableNgramsApiGet
  , TableNgramsApiPut
  , TableNgramsApiPost

  , getTableNgrams
  , setListNgrams
  , rmListNgrams
  , putListNgrams
  , putListNgrams'
  , tableNgramsPost
  , apiNgramsTableCorpus
  , apiNgramsTableDoc

  , NgramsStatePatch
  , NgramsTablePatch
  , NgramsTableMap

  , NgramsElement(..)
  , mkNgramsElement
  , mergeNgramsElement

  , RootParent(..)

  , MSet
  , mSetFromList
  , mSetToList

  , Repo(..)
  , r_version
  , r_state
  , r_history
  , NgramsRepo
  , NgramsRepoElement(..)
  , saveRepo
  , initRepo

  , RepoEnv(..)
  , renv_var
  , renv_lock

  , TabType(..)
  , ngramsTypeFromTabType

  , HasRepoVar(..)
  , HasRepoSaver(..)
  , HasRepo(..)
  , RepoCmdM
  , QueryParamR
  , TODO

  -- Internals
  , getNgramsTableMap
  , dumpJsonTableMap
  , tableNgramsPull
  , tableNgramsPut

  , Version
  , Versioned(..)
  , currentVersion
  , listNgramsChangedSince
  )
  where

import Codec.Serialise (Serialise())
import Control.Category ((>>>))
import Control.Concurrent
import Control.Lens (makeLenses, makePrisms, Getter, Iso', iso, from, (.~), (?=), (#), to, folded, {-withIndex, ifolded,-} view, use, (^.), (^..), (^?), (+~), (%~), (.~), (%=), sumOf, at, _Just, Each(..), itraverse_, both, forOf_, (%%~), (?~), mapped)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson hiding ((.=))
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Text as DAT
import Data.Either (Either(Left))
import Data.Foldable
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.Patch as PM
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord (Down(..))
import Data.Patch.Class (Replace, replace, Action(act), Applicable(..), Composable(..), Transformable(..), PairPatch(..), Patched, ConflictResolution, ConflictResolutionReplace, ours)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.Swagger hiding (version, patch)
import Data.Text (Text, count, isInfixOf, unpack)
import Data.Text.Lazy.IO as DTL
import Data.Validity
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Formatting (hprint, int, (%))
import Formatting.Clock (timeSpecs)
import GHC.Generics (Generic)
import Servant hiding (Patch)
import System.Clock (getTime, TimeSpec, Clock(..))
import System.FileLock (FileLock)
import System.IO (stderr)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Prelude (error)
import Protolude (maybeToEither)
import Gargantext.Prelude

import Gargantext.Core.Types (ListType(..), NodeId, ListId, DocId, Limit, Offset, HasInvalidError, assertValid)
import Gargantext.Core.Types (TODO)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger, wellNamedSchema)
import Gargantext.Database.Action.Metrics.NgramsByNode (getOccByNgramsOnlyFast')
import Gargantext.Database.Query.Table.Node.Select
import Gargantext.Database.Query.Table.Ngrams hiding (NgramsType(..), ngrams, ngramsType, ngrams_terms)
import Gargantext.Database.Admin.Config (userMaster)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Admin.Types.Node (NodeType(..))
import Gargantext.Database.Prelude (fromField', HasConnectionPool, HasConfig)
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams

------------------------------------------------------------------------
--data FacetFormat = Table | Chart
data TabType   = Docs   | Trash   | MoreFav | MoreTrash
               | Terms  | Sources | Authors | Institutes
               | Contacts
  deriving (Generic, Enum, Bounded, Show)

instance FromHttpApiData TabType
   where
    parseUrlPiece "Docs"       = pure Docs
    parseUrlPiece "Trash"      = pure Trash
    parseUrlPiece "MoreFav"    = pure MoreFav
    parseUrlPiece "MoreTrash"  = pure MoreTrash

    parseUrlPiece "Terms"      = pure Terms
    parseUrlPiece "Sources"    = pure Sources
    parseUrlPiece "Institutes" = pure Institutes
    parseUrlPiece "Authors"    = pure Authors

    parseUrlPiece "Contacts"   = pure Contacts

    parseUrlPiece _            = Left "Unexpected value of TabType"

instance ToParamSchema TabType
instance ToJSON        TabType
instance FromJSON      TabType
instance ToSchema      TabType
instance Arbitrary     TabType
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

-- mSetToSet :: Ord a => MSet a -> Set a
-- mSetToSet (MSet a) = Set.fromList ( Map.keys a)
mSetToSet :: Ord a => MSet a -> Set a
mSetToSet = Set.fromList . mSetToList

mSetToList :: MSet a -> [a]
mSetToList (MSet a) = Map.keys a

instance Foldable MSet where
  foldMap f (MSet m) = Map.foldMapWithKey (\k _ -> f k) m

instance (Ord a, FromJSON a) => FromJSON (MSet a) where
  parseJSON = fmap mSetFromList . parseJSON

instance (ToJSONKey a, ToSchema a) => ToSchema (MSet a) where
  -- TODO
  declareNamedSchema _ = wellNamedSchema "" (Proxy :: Proxy TODO)

------------------------------------------------------------------------
type NgramsTerm = Text

data RootParent = RootParent
  { _rp_root   :: NgramsTerm
  , _rp_parent :: NgramsTerm
  }
  deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_rp_") ''RootParent
makeLenses ''RootParent

data NgramsRepoElement = NgramsRepoElement
  { _nre_size        :: Int
  , _nre_list        :: ListType
--, _nre_root_parent :: Maybe RootParent
  , _nre_root        :: Maybe NgramsTerm
  , _nre_parent      :: Maybe NgramsTerm
  , _nre_children    :: MSet NgramsTerm
  }
  deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_nre_") ''NgramsRepoElement
makeLenses ''NgramsRepoElement

instance ToSchema NgramsRepoElement where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_nre_")

instance Serialise (MSet NgramsTerm)
instance Serialise NgramsRepoElement

data NgramsElement =
     NgramsElement { _ne_ngrams      :: NgramsTerm
                   , _ne_size        :: Int
                   , _ne_list        :: ListType
                   , _ne_occurrences :: Int
                   , _ne_root        :: Maybe NgramsTerm
                   , _ne_parent      :: Maybe NgramsTerm
                   , _ne_children    :: MSet  NgramsTerm
                   }
      deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_ne_") ''NgramsElement
makeLenses ''NgramsElement

mkNgramsElement :: NgramsTerm
                -> ListType
                -> Maybe RootParent
                -> MSet NgramsTerm
                -> NgramsElement
mkNgramsElement ngrams list rp children =
  NgramsElement ngrams size list 1 (_rp_root <$> rp) (_rp_parent <$> rp) children
  where
    -- TODO review
    size = 1 + count " " ngrams

newNgramsElement :: Maybe ListType -> NgramsTerm -> NgramsElement
newNgramsElement mayList ngrams =
  mkNgramsElement ngrams (fromMaybe MapTerm mayList) Nothing mempty

instance ToSchema NgramsElement where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ne_")
instance Arbitrary NgramsElement where
  arbitrary = elements [newNgramsElement Nothing "sport"]

ngramsElementToRepo :: NgramsElement -> NgramsRepoElement
ngramsElementToRepo
  (NgramsElement { _ne_size     = s
                 , _ne_list     = l
                 , _ne_root     = r
                 , _ne_parent   = p
                 , _ne_children = c
                 }) =
  NgramsRepoElement
    { _nre_size     = s
    , _nre_list     = l
    , _nre_parent   = p
    , _nre_root     = r
    , _nre_children = c
    }

ngramsElementFromRepo :: NgramsTerm -> NgramsRepoElement -> NgramsElement
ngramsElementFromRepo
  ngrams
  (NgramsRepoElement
      { _nre_size     = s
      , _nre_list     = l
      , _nre_parent   = p
      , _nre_root     = r
      , _nre_children = c
      }) =
  NgramsElement { _ne_size        = s
                , _ne_list        = l
                , _ne_root        = r
                , _ne_parent      = p
                , _ne_children    = c
                , _ne_ngrams      = ngrams
                , _ne_occurrences = panic $ "API.Ngrams._ne_occurrences"
                {-
                -- Here we could use 0 if we want to avoid any `panic`.
                -- It will not happen using getTableNgrams if
                -- getOccByNgramsOnly provides a count of occurrences for
                -- all the ngrams given.
                -}
                }

------------------------------------------------------------------------
newtype NgramsTable = NgramsTable [NgramsElement]
  deriving (Ord, Eq, Generic, ToJSON, FromJSON, Show)

type NgramsList = NgramsTable

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
  [ mkNgramsElement "animal"  MapTerm      Nothing       (mSetFromList ["dog", "cat"])
  , mkNgramsElement "cat"     MapTerm     (rp "animal")  mempty
  , mkNgramsElement "cats"    StopTerm       Nothing       mempty
  , mkNgramsElement "dog"     MapTerm     (rp "animal")  (mSetFromList ["dogs"])
  , mkNgramsElement "dogs"    StopTerm      (rp "dog")     mempty
  , mkNgramsElement "fox"     MapTerm      Nothing       mempty
  , mkNgramsElement "object"  CandidateTerm  Nothing       mempty
  , mkNgramsElement "nothing" StopTerm       Nothing       mempty
  , mkNgramsElement "organic" MapTerm      Nothing       (mSetFromList ["flower"])
  , mkNgramsElement "flower"  MapTerm     (rp "organic") mempty
  , mkNgramsElement "moon"    CandidateTerm  Nothing       mempty
  , mkNgramsElement "sky"     StopTerm       Nothing       mempty
  ]
  where
    rp n = Just $ RootParent n n

instance Arbitrary NgramsTable where
  arbitrary = pure mockTable

instance ToSchema NgramsTable

------------------------------------------------------------------------
type NgramsTableMap = Map NgramsTerm NgramsRepoElement
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

instance Serialise AddRem

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

instance (Serialise a, Ord a) => Serialise (PatchMap a AddRem)
instance (Serialise a, Ord a) => Serialise (PatchMSet a)

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
  declareNamedSchema _ = wellNamedSchema "" (Proxy :: Proxy TODO)

type instance Patched (PatchMSet a) = MSet a

instance (Eq a, Arbitrary a) => Arbitrary (Replace a) where
  arbitrary = uncurry replace <$> arbitrary
    -- If they happen to be equal then the patch is Keep.

instance ToSchema a => ToSchema (Replace a) where
  declareNamedSchema (_ :: Proxy (Replace a)) = do
    -- TODO Keep constructor is not supported here.
    aSchema <- declareSchemaRef (Proxy :: Proxy a)
    return $ NamedSchema (Just "Replace") $ mempty
            & type_ ?~ SwaggerObject
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

instance ToSchema  NgramsPatch where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_")

instance Arbitrary NgramsPatch where
  arbitrary = NgramsPatch <$> arbitrary <*> (replace <$> arbitrary <*> arbitrary)

instance Serialise NgramsPatch
instance Serialise (Replace ListType)
instance Serialise ListType

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

instance Applicable NgramsPatch (Maybe NgramsRepoElement) where
  applicable p Nothing   = check (p == mempty) "NgramsPatch should be empty here"
  applicable p (Just nre) =
    applicable (p ^. patch_children) (nre ^. nre_children) <>
    applicable (p ^. patch_list)     (nre ^. nre_list)

instance Action NgramsPatch NgramsRepoElement where
  act p = (nre_children %~ act (p ^. patch_children))
        . (nre_list     %~ act (p ^. patch_list))

instance Action NgramsPatch (Maybe NgramsRepoElement) where
  act = fmap . act

newtype NgramsTablePatch = NgramsTablePatch (PatchMap NgramsTerm NgramsPatch)
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Semigroup, Monoid, Validity, Transformable)

instance Serialise NgramsTablePatch
instance Serialise (PatchMap NgramsTerm NgramsPatch)

instance FromField NgramsTablePatch
  where
    fromField = fromField'

instance FromField (PatchMap TableNgrams.NgramsType (PatchMap NodeId NgramsTablePatch))
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

reRootChildren :: NgramsTerm -> ReParent NgramsTerm
reRootChildren root ngram = do
  nre <- use $ at ngram
  forOf_ (_Just . nre_children . folded) nre $ \child -> do
    at child . _Just . nre_root ?= root
    reRootChildren root child

reParent :: Maybe RootParent -> ReParent NgramsTerm
reParent rp child = do
  at child . _Just %= ( (nre_parent .~ (_rp_parent <$> rp))
                      . (nre_root   .~ (_rp_root   <$> rp))
                      )
  reRootChildren (fromMaybe child (rp ^? _Just . rp_root)) child

reParentAddRem :: RootParent -> NgramsTerm -> ReParent AddRem
reParentAddRem rp child p =
  reParent (if isRem p then Nothing else Just rp) child

reParentNgramsPatch :: NgramsTerm -> ReParent NgramsPatch
reParentNgramsPatch parent ngramsPatch = do
  root_of_parent <- use (at parent . _Just . nre_root)
  let
    root = fromMaybe parent root_of_parent
    rp   = RootParent { _rp_root = root, _rp_parent = parent }
  itraverse_ (reParentAddRem rp) (ngramsPatch ^. patch_children . _PatchMSet . _PatchMap)
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
  deriving (Generic, Show, Eq)
deriveJSON (unPrefix "_v_") ''Versioned
makeLenses ''Versioned
instance (Typeable a, ToSchema a) => ToSchema (Versioned a) where
  declareNamedSchema = wellNamedSchema "_v_"
instance Arbitrary a => Arbitrary (Versioned a) where
  arbitrary = Versioned 1 <$> arbitrary -- TODO 1 is constant so far


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

ngramsTypeFromTabType :: TabType -> TableNgrams.NgramsType
ngramsTypeFromTabType tabType =
  let lieu = "Garg.API.Ngrams: " :: Text in
    case tabType of
      Sources    -> TableNgrams.Sources
      Authors    -> TableNgrams.Authors
      Institutes -> TableNgrams.Institutes
      Terms      -> TableNgrams.NgramsTerms
      _          -> panic $ lieu <> "No Ngrams for this tab"
      -- TODO: This `panic` would disapear with custom NgramsType.

------------------------------------------------------------------------
data Repo s p = Repo
  { _r_version :: Version
  , _r_state   :: s
  , _r_history :: [p]
    -- first patch in the list is the most recent
  }
  deriving (Generic)

instance (FromJSON s, FromJSON p) => FromJSON (Repo s p) where
  parseJSON = genericParseJSON $ unPrefix "_r_"

instance (ToJSON s, ToJSON p) => ToJSON (Repo s p) where
  toJSON     = genericToJSON     $ unPrefix "_r_"
  toEncoding = genericToEncoding $ unPrefix "_r_"

instance (Serialise s, Serialise p) => Serialise (Repo s p)

makeLenses ''Repo

initRepo :: Monoid s => Repo s p
initRepo = Repo 1 mempty []

type NgramsRepo       = Repo NgramsState NgramsStatePatch
type NgramsState      = Map      TableNgrams.NgramsType (Map NodeId NgramsTableMap)
type NgramsStatePatch = PatchMap TableNgrams.NgramsType (PatchMap NodeId NgramsTablePatch)

instance Serialise (PM.PatchMap NodeId NgramsTablePatch)
instance Serialise NgramsStatePatch

initMockRepo :: NgramsRepo
initMockRepo = Repo 1 s []
  where
    s = Map.singleton TableNgrams.NgramsTerms
      $ Map.singleton 47254
      $ Map.fromList
      [ (n ^. ne_ngrams, ngramsElementToRepo n) | n <- mockTable ^. _NgramsTable ]

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

type RepoCmdM   env err m =
  ( MonadReader env     m
  , MonadError      err m
  , MonadBaseControl IO m
  , HasRepo     env
  )
------------------------------------------------------------------------

saveRepo :: ( MonadReader env m, MonadBase IO m, HasRepoSaver env )
         => m ()
saveRepo = liftBase =<< view repoSaver

listTypeConflictResolution :: ListType -> ListType -> ListType
listTypeConflictResolution _ _ = undefined -- TODO Use Map User ListType

ngramsStatePatchConflictResolution
  :: TableNgrams.NgramsType
  -> NodeId
  -> NgramsTerm
  -> ConflictResolutionNgramsPatch
ngramsStatePatchConflictResolution _ngramsType _nodeId _ngramsTerm
  = (const ours, ours)
  -- undefined {- TODO think this through -}, listTypeConflictResolution)

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
  liftBase $ modifyMVar_ var $
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
  liftBase $ modifyMVar_ var $
    pure . (r_state . at ngramsType . _Just . at listId . _Just <>~ m)
  saveRepo
  where
    m = Map.fromList $ (\n -> (n ^. ne_ngrams, n)) <$> nes
-}

rmListNgrams ::  RepoCmdM env err m
              => ListId
              -> TableNgrams.NgramsType
              -> m ()
rmListNgrams l nt = setListNgrams l nt mempty

-- | TODO: incr the Version number
-- && should use patch
setListNgrams ::  RepoCmdM env err m
              => NodeId
              -> TableNgrams.NgramsType
              -> Map NgramsTerm NgramsRepoElement
              -> m ()
setListNgrams listId ngramsType ns = do
  var <- view repoVar
  liftBase $ modifyMVar_ var $
    pure . ( r_state
           . at ngramsType %~
             (Just .
               (at listId .~ ( Just ns))
               . something
             )
           )
  saveRepo


-- If the given list of ngrams elements contains ngrams already in
-- the repo, they will be ignored.
putListNgrams :: RepoCmdM env err m
              => NodeId
              -> TableNgrams.NgramsType
              -> [NgramsElement] -> m ()
putListNgrams _ _ [] = pure ()
putListNgrams nodeId ngramsType nes = putListNgrams' nodeId ngramsType m
  where
    m = Map.fromList $ map (\n -> (n ^. ne_ngrams, ngramsElementToRepo n)) nes

putListNgrams' :: RepoCmdM env err m
               => NodeId
               -> TableNgrams.NgramsType
               -> Map NgramsTerm NgramsRepoElement
               -> m ()
putListNgrams' nodeId ngramsType ns = do
  -- printDebug "[putLictNgrams'] nodeId" nodeId
  -- printDebug "[putLictNgrams'] ngramsType" ngramsType
  -- printDebug "[putListNgrams'] ns" ns
  var <- view repoVar
  liftBase $ modifyMVar_ var $ \r -> do
    pure $ r & r_version +~ 1
             & r_history %~ (mempty :)
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


-- TODO-ACCESS check
tableNgramsPost :: RepoCmdM env err m
                => TabType
                -> NodeId
                -> Maybe ListType
                -> [NgramsTerm] -> m ()
tableNgramsPost tabType nodeId mayList =
  putListNgrams nodeId (ngramsTypeFromTabType tabType) . fmap (newNgramsElement mayList)

currentVersion :: RepoCmdM env err m
               => m Version
currentVersion = do
  var <- view repoVar
  r   <- liftBase $ readMVar var
  pure $ r ^. r_version

tableNgramsPull :: RepoCmdM env err m
                => ListId
                -> TableNgrams.NgramsType
                -> Version
                -> m (Versioned NgramsTablePatch)
tableNgramsPull listId ngramsType p_version = do
  var <- view repoVar
  r <- liftBase $ readMVar var

  let
    q = mconcat $ take (r ^. r_version - p_version) (r ^. r_history)
    q_table = q ^. _PatchMap . at ngramsType . _Just . _PatchMap . at listId . _Just

  pure (Versioned (r ^. r_version) q_table)

-- Apply the given patch to the DB and returns the patch to be applied on the
-- client.
-- TODO-ACCESS check
tableNgramsPut :: (HasInvalidError err, RepoCmdM env err m)
                 => TabType -> ListId
                 -> Versioned NgramsTablePatch
                 -> m (Versioned NgramsTablePatch)
tableNgramsPut tabType listId (Versioned p_version p_table)
  | p_table == mempty = do
      let ngramsType        = ngramsTypeFromTabType tabType
      tableNgramsPull listId ngramsType p_version

  | otherwise         = do
      let ngramsType        = ngramsTypeFromTabType tabType
          (p0, p0_validity) = PM.singleton listId p_table
          (p, p_validity)   = PM.singleton ngramsType p0

      assertValid p0_validity
      assertValid p_validity

      var <- view repoVar
      vq' <- liftBase $ modifyMVar var $ \r -> do
        let
          q = mconcat $ take (r ^. r_version - p_version) (r ^. r_history)
          (p', q') = transformWith ngramsStatePatchConflictResolution p q
          r' = r & r_version +~ 1
                 & r_state   %~ act p'
                 & r_history %~ (p' :)
          q'_table = q' ^. _PatchMap . at ngramsType . _Just . _PatchMap . at listId . _Just
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
        pure (r', Versioned (r' ^. r_version) q'_table)

      saveRepo
      pure vq'

mergeNgramsElement :: NgramsRepoElement -> NgramsRepoElement -> NgramsRepoElement
mergeNgramsElement _neOld neNew = neNew
  {-
  { _ne_list        :: ListType
  If we merge the parents/children we can potentially create cycles!
  , _ne_parent      :: Maybe NgramsTerm
  , _ne_children    :: MSet NgramsTerm
  }
  -}

getNgramsTableMap :: RepoCmdM env err m
                  => NodeId
                  -> TableNgrams.NgramsType
                  -> m (Versioned NgramsTableMap)
getNgramsTableMap nodeId ngramsType = do
  v    <- view repoVar
  repo <- liftBase $ readMVar v
  pure $ Versioned (repo ^. r_version)
                   (repo ^. r_state . at ngramsType . _Just . at nodeId . _Just)

dumpJsonTableMap :: RepoCmdM env err m
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

getTime' :: MonadBase IO m => m TimeSpec
getTime' = liftBase $ getTime ProcessCPUTime


getTableNgrams :: forall env err m.
                  (RepoCmdM env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => NodeType -> NodeId -> TabType
               -> ListId -> Limit -> Maybe Offset
               -> Maybe ListType
               -> Maybe MinSize -> Maybe MaxSize
               -> Maybe OrderBy
               -> (NgramsTerm -> Bool)
               -> m (Versioned NgramsTable)
getTableNgrams _nType nId tabType listId limit_ offset
               listType minSize maxSize orderBy searchQuery = do

  t0 <- getTime'
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
    selectAndPaginate :: Map NgramsTerm NgramsElement -> [NgramsElement]
    selectAndPaginate tableMap = roots <> inners
      where
        list = tableMap ^.. each
        rootOf ne = maybe ne (\r -> fromMaybe (panic "getTableNgrams: invalid root") (tableMap ^. at r))
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
      let ngrams_terms = (table ^.. each . ne_ngrams)
      t1 <- getTime'
      occurrences <- getOccByNgramsOnlyFast' nId
                                             listId
                                            ngramsType
                                            ngrams_terms
      t2 <- getTime'
      liftBase $ hprint stderr
        ("getTableNgrams/setScores #ngrams=" % int % " time=" % timeSpecs % "\n")
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
  t1 <- getTime'
  tableMap2 <- tableMap1 & v_data %%~ setScores scoresNeeded
                                    . Map.mapWithKey ngramsElementFromRepo
  t2 <- getTime'
  tableMap3 <- tableMap2 & v_data %%~ fmap NgramsTable
                                    . setScores (not scoresNeeded)
                                    . selectAndPaginate
  t3 <- getTime'
  liftBase $ hprint stderr
            ("getTableNgrams total=" % timeSpecs
                          % " map1=" % timeSpecs
                          % " map2=" % timeSpecs
                          % " map3=" % timeSpecs
                          % " sql="  % (if scoresNeeded then "map2" else "map3")
                          % "\n"
            ) t0 t3 t0 t1 t1 t2 t2 t3
  pure tableMap3


scoresRecomputeTableNgrams :: forall env err m. (RepoCmdM env err m, HasNodeError err, HasConnectionPool env, HasConfig env) => NodeId -> TabType -> ListId -> m Int
scoresRecomputeTableNgrams nId tabType listId = do
  tableMap <- getNgramsTableMap listId ngramsType
  _ <- tableMap & v_data %%~ setScores
                           . Map.mapWithKey ngramsElementFromRepo

  pure $ 1
  where
    ngramsType = ngramsTypeFromTabType tabType

    setScores :: forall t. Each t t NgramsElement NgramsElement => t -> m t
    setScores table = do
      let ngrams_terms = (table ^.. each . ne_ngrams)
      occurrences <- getOccByNgramsOnlyFast' nId
                                             listId
                                            ngramsType
                                            ngrams_terms
      let
        setOcc ne = ne & ne_occurrences .~ sumOf (at (ne ^. ne_ngrams) . _Just) occurrences

      pure $ table & each %~ setOcc



-- APIs

-- TODO: find a better place for the code above, All APIs stay here
type QueryParamR = QueryParam' '[Required, Strict]

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
                      :> Get    '[JSON] (Versioned NgramsTable)

type TableNgramsApiPut = Summary " Table Ngrams API Change"
                       :> QueryParamR "ngramsType" TabType
                       :> QueryParamR "list"       ListId
                       :> ReqBody '[JSON] (Versioned NgramsTablePatch)
                       :> Put     '[JSON] (Versioned NgramsTablePatch)

type TableNgramsApiPost = Summary " Table Ngrams API Adds new ngrams"
                       :> QueryParamR "ngramsType" TabType
                       :> QueryParamR "list"       ListId
                       :> QueryParam  "listType"   ListType
                       :> ReqBody '[JSON] [NgramsTerm]
                       :> Post    '[JSON] ()

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
                  :<|> TableNgramsApiPost
                  :<|> RecomputeScoresNgramsApiGet
                  :<|> "version" :> TableNgramsApiGetVersion

getTableNgramsCorpus :: (RepoCmdM env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => NodeId
               -> TabType
               -> ListId
               -> Limit
               -> Maybe Offset
               -> Maybe ListType
               -> Maybe MinSize -> Maybe MaxSize
               -> Maybe OrderBy
               -> Maybe Text -- full text search
               -> m (Versioned NgramsTable)
getTableNgramsCorpus nId tabType listId limit_ offset listType minSize maxSize orderBy mt =
  getTableNgrams NodeCorpus nId tabType listId limit_ offset listType minSize maxSize orderBy searchQuery
    where
      searchQuery = maybe (const True) isInfixOf mt

getTableNgramsVersion :: (RepoCmdM env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => NodeId
               -> TabType
               -> ListId
               -> m Version
getTableNgramsVersion nId tabType listId = do
  -- TODO: limit?
  Versioned { _v_version = v } <- getTableNgramsCorpus nId tabType listId 100000 Nothing Nothing Nothing Nothing Nothing Nothing
  pure v

-- | Text search is deactivated for now for ngrams by doc only
getTableNgramsDoc :: (RepoCmdM env err m, HasNodeError err, HasConnectionPool env, HasConfig env)
               => DocId -> TabType
               -> ListId -> Limit -> Maybe Offset
               -> Maybe ListType
               -> Maybe MinSize -> Maybe MaxSize
               -> Maybe OrderBy
               -> Maybe Text -- full text search
               -> m (Versioned NgramsTable)
getTableNgramsDoc dId tabType listId limit_ offset listType minSize maxSize orderBy _mt = do
  ns <- selectNodesWithUsername NodeList userMaster
  let ngramsType = ngramsTypeFromTabType tabType
  ngs <- selectNgramsByDoc (ns <> [listId]) dId ngramsType
  let searchQuery = flip S.member (S.fromList ngs)
  getTableNgrams NodeDocument dId tabType listId limit_ offset listType minSize maxSize orderBy searchQuery



apiNgramsTableCorpus :: ( RepoCmdM env err m
                        , HasNodeError err
                        , HasInvalidError err
                        , HasConnectionPool env
                        , HasConfig         env
                        )
                     => NodeId -> ServerT TableNgramsApi m
apiNgramsTableCorpus cId =  getTableNgramsCorpus cId
                       :<|> tableNgramsPut
                       :<|> tableNgramsPost
                       :<|> scoresRecomputeTableNgrams cId
                       :<|> getTableNgramsVersion cId

apiNgramsTableDoc :: ( RepoCmdM env err m
                     , HasNodeError err
                     , HasInvalidError err
                     , HasConnectionPool env
                     , HasConfig         env
                     )
                  => DocId -> ServerT TableNgramsApi m
apiNgramsTableDoc dId =  getTableNgramsDoc dId
                    :<|> tableNgramsPut
                    :<|> tableNgramsPost
                    :<|> scoresRecomputeTableNgrams dId
                    :<|> getTableNgramsVersion dId
                    -- > add new ngrams in database (TODO AD)
                    -- > index all the corpus accordingly (TODO AD)

listNgramsChangedSince :: RepoCmdM env err m
                       => ListId -> TableNgrams.NgramsType -> Version -> m (Versioned Bool)
listNgramsChangedSince listId ngramsType version
  | version < 0 =
      Versioned <$> currentVersion <*> pure True
  | otherwise   =
      tableNgramsPull listId ngramsType version & mapped . v_data %~ (== mempty)

-- Instances
instance Arbitrary NgramsRepoElement where
  arbitrary = elements $ map ngramsElementToRepo ns
    where
      NgramsTable ns = mockTable

--{-
instance FromHttpApiData (Map TableNgrams.NgramsType (Versioned NgramsTableMap))
  where
    parseUrlPiece x = maybeToEither x (decode $ cs x)
