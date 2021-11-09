-- |

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans #-}

module Gargantext.API.Ngrams.Types where

import Codec.Serialise (Serialise())
import Control.Category ((>>>))
import Control.Lens (makeLenses, makePrisms, Iso', iso, from, (.~), (?=), (#), to, folded, {-withIndex, ifolded,-} view, use, (^.), (^?), (%~), (.~), (%=), at, _Just, Each(..), itraverse_, both, forOf_, (?~))
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Aeson.TH (deriveJSON)
import Data.Either (Either(..))
import Data.Foldable
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Patch.Class (Replace, replace, Action(act), Group, Applicable(..), Composable(..), Transformable(..), PairPatch(..), Patched, ConflictResolution, ConflictResolutionReplace, MaybePatch(Mod), unMod, old, new)
import Data.Set (Set)
import Data.String (IsString, fromString)
import Data.Swagger hiding (version, patch)
import Data.Text (Text, pack, strip)
import Data.Validity
import Database.PostgreSQL.Simple.FromField (FromField, fromField, ResultError(ConversionFailed), returnError)
import GHC.Generics (Generic)
import Gargantext.Core.Text (size)
import Gargantext.Core.Types (ListType(..), ListId, NodeId, TODO)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixUntagged, unPrefixSwagger, wellNamedSchema)
import Gargantext.Database.Prelude (fromField', HasConnectionPool, HasConfig, CmdM')
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (IsHashable(..))
import Protolude (maybeToEither)
import Servant hiding (Patch)
import Servant.Job.Utils (jsonOptions)
-- import System.FileLock (FileLock)
import Test.QuickCheck (elements, frequency)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Data.HashMap.Strict.InsOrd             as InsOrdHashMap
import qualified Data.List                              as List
import qualified Data.Map.Strict                        as Map
import qualified Data.Map.Strict.Patch                  as PM
import qualified Data.Set                               as Set
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams

------------------------------------------------------------------------

type QueryParamR = QueryParam' '[Required, Strict]

------------------------------------------------------------------------
--data FacetFormat = Table | Chart
data TabType   = Docs   | Trash   | MoreFav | MoreTrash
               | Terms  | Sources | Authors | Institutes
               | Contacts
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)


instance Hashable TabType

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
instance Arbitrary     TabType where
  arbitrary = elements [minBound .. maxBound]
instance FromJSONKey TabType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey TabType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

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
newtype NgramsTerm = NgramsTerm { unNgramsTerm :: Text }
  deriving (Ord, Eq, Show, Generic, ToJSONKey, ToJSON, FromJSON, Semigroup, Arbitrary, Serialise, ToSchema, Hashable)

instance IsHashable NgramsTerm where
  hash (NgramsTerm t) = hash t

instance Monoid NgramsTerm where
  mempty = NgramsTerm ""

instance FromJSONKey NgramsTerm where
  fromJSONKey = FromJSONKeyTextParser $ \t -> pure $ NgramsTerm $ strip t

instance IsString NgramsTerm where
  fromString s = NgramsTerm $ pack s

instance FromField NgramsTerm
  where
    fromField field mb = do
      v <- fromField field mb
      case fromJSON v of
        Success a -> pure $ NgramsTerm $ strip a
        Error _err -> returnError ConversionFailed field
                      $ List.intercalate " " [ "cannot parse hyperdata for JSON: "
                                             , show v
                                             ]

data RootParent = RootParent
  { _rp_root   :: NgramsTerm
  , _rp_parent :: NgramsTerm
  }
  deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_rp_") ''RootParent
makeLenses ''RootParent

data NgramsRepoElement = NgramsRepoElement
  { _nre_size        :: !Int
  , _nre_list        :: !ListType
  , _nre_root        :: !(Maybe NgramsTerm)
  , _nre_parent      :: !(Maybe NgramsTerm)
  , _nre_children    :: !(MSet NgramsTerm)
  }
  deriving (Ord, Eq, Show, Generic)

deriveJSON (unPrefix "_nre_") ''NgramsRepoElement
-- TODO
-- if ngrams & not size => size
-- drop occurrences

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
  NgramsElement ngrams (size (unNgramsTerm ngrams)) list 1 (_rp_root <$> rp) (_rp_parent <$> rp) children

newNgramsElement :: Maybe ListType -> NgramsTerm -> NgramsElement
newNgramsElement mayList ngrams =
  mkNgramsElement ngrams (fromMaybe MapTerm mayList) Nothing mempty

instance ToSchema NgramsElement where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_ne_")
instance Arbitrary NgramsElement where
  arbitrary = elements [newNgramsElement Nothing "sport"]


------------------------------------------------------------------------
newtype NgramsTable = NgramsTable [NgramsElement]
  deriving (Ord, Eq, Generic, ToJSON, FromJSON, Show)

-- type NgramsList = NgramsTable

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
  [ mkNgramsElement "animal"  MapTerm        Nothing       (mSetFromList ["dog", "cat"])
  , mkNgramsElement "cat"     MapTerm       (rp "animal")  mempty
  , mkNgramsElement "cats"    StopTerm       Nothing       mempty
  , mkNgramsElement "dog"     MapTerm       (rp "animal")  (mSetFromList ["dogs"])
  , mkNgramsElement "dogs"    StopTerm      (rp "dog")     mempty
  , mkNgramsElement "fox"     MapTerm        Nothing       mempty
  , mkNgramsElement "object"  CandidateTerm  Nothing       mempty
  , mkNgramsElement "nothing" StopTerm       Nothing       mempty
  , mkNgramsElement "organic" MapTerm        Nothing       (mSetFromList ["flower"])
  , mkNgramsElement "flower"  MapTerm       (rp "organic") mempty
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
  deriving (Eq, Show, Generic, Validity, Semigroup, Monoid, Group,
            Transformable, Composable)

unPatchMSet :: PatchMSet a -> PatchMap a AddRem
unPatchMSet (PatchMSet a) = a

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

data NgramsPatch
   = NgramsPatch { _patch_children :: !(PatchMSet NgramsTerm)
                 , _patch_list     :: !(Replace ListType)   -- TODO Map UserId ListType
                 }
   | NgramsReplace { _patch_old :: !(Maybe NgramsRepoElement)
                   , _patch_new :: !(Maybe NgramsRepoElement)
                   }
      deriving (Eq, Show, Generic)

-- The JSON encoding is untagged, this is OK since the field names are disjoints and thus the encoding is unambiguous.
-- TODO: the empty object should be accepted and treated as mempty.
deriveJSON (unPrefixUntagged "_") ''NgramsPatch
makeLenses ''NgramsPatch

-- TODO: This instance is simplified since we should either have the fields children and/or list
-- or the fields old and/or new.
instance ToSchema NgramsPatch where
  declareNamedSchema _ = do
    childrenSch <- declareSchemaRef (Proxy :: Proxy (PatchMSet NgramsTerm))
    listSch <- declareSchemaRef (Proxy :: Proxy (Replace ListType))
    nreSch <- declareSchemaRef (Proxy :: Proxy NgramsRepoElement)
    return $ NamedSchema (Just "NgramsPatch") $ mempty
            & type_ ?~ SwaggerObject
            & properties .~
                InsOrdHashMap.fromList
                [ ("children", childrenSch)
                , ("list",     listSch)
                , ("old",      nreSch)
                , ("new",      nreSch)
                ]

instance Arbitrary NgramsPatch where
  arbitrary = frequency [ (9, NgramsPatch <$> arbitrary <*> (replace <$> arbitrary <*> arbitrary))
                        , (1, NgramsReplace <$> arbitrary <*> arbitrary)
                        ]

instance Serialise NgramsPatch
instance Serialise (Replace ListType)

instance Serialise ListType

type NgramsPatchIso =
  MaybePatch NgramsRepoElement (PairPatch (PatchMSet NgramsTerm) (Replace ListType))

_NgramsPatch :: Iso' NgramsPatch NgramsPatchIso
_NgramsPatch = iso unwrap wrap
  where
    unwrap (NgramsPatch c l) = Mod $ PairPatch (c, l)
    unwrap (NgramsReplace o n) = replace o n
    wrap x =
      case unMod x of
        Just (PairPatch (c, l)) -> NgramsPatch c l
        Nothing -> NgramsReplace (x ^? old . _Just) (x ^? new . _Just)

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
  ( ConflictResolutionReplace (Maybe NgramsRepoElement)
  , ( ConflictResolutionPatchMSet NgramsTerm
    , ConflictResolutionReplace ListType
    )
  , (Bool, Bool)
  )
type instance ConflictResolution NgramsPatch =
  ConflictResolutionNgramsPatch

type PatchedNgramsPatch = Maybe NgramsRepoElement
type instance Patched NgramsPatch = PatchedNgramsPatch

instance Applicable (PairPatch (PatchMSet NgramsTerm) (Replace ListType)) NgramsRepoElement where
  applicable (PairPatch (c, l)) n = applicable c (n ^. nre_children) <> applicable l (n ^. nre_list)

instance Action (PairPatch (PatchMSet NgramsTerm) (Replace ListType)) NgramsRepoElement where
  act (PairPatch (c, l)) = (nre_children %~ act c)
                         . (nre_list     %~ act l)

instance Applicable NgramsPatch (Maybe NgramsRepoElement) where
  applicable p = applicable (p ^. _NgramsPatch)

instance Action NgramsPatch (Maybe NgramsRepoElement) where
  act p = act (p ^. _NgramsPatch)

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
                , _ne_occurrences = panic $ "API.Ngrams.Types._ne_occurrences"
                {-
                -- Here we could use 0 if we want to avoid any `panic`.
                -- It will not happen using getTableNgrams if
                -- getOccByNgramsOnly provides a count of occurrences for
                -- all the ngrams given.
                -}
                }

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
------------------------------------------------------------------------
type Count = Int

data VersionedWithCount a = VersionedWithCount
  { _vc_version :: Version
  , _vc_count   :: Count
  , _vc_data    :: a
  }
  deriving (Generic, Show, Eq)
deriveJSON (unPrefix "_vc_") ''VersionedWithCount
makeLenses ''VersionedWithCount
instance (Typeable a, ToSchema a) => ToSchema (VersionedWithCount a) where
  declareNamedSchema = wellNamedSchema "_vc_"
instance Arbitrary a => Arbitrary (VersionedWithCount a) where
  arbitrary = VersionedWithCount 1 1 <$> arbitrary -- TODO 1 is constant so far

toVersionedWithCount :: Count -> Versioned a -> VersionedWithCount a
toVersionedWithCount count (Versioned version data_) = VersionedWithCount version count data_
------------------------------------------------------------------------

-- | TOREMOVE
data Repo s p = Repo
  { _r_version :: !Version
  , _r_state   :: !s
  , _r_history :: ![p]
    -- first patch in the list is the most recent
  }
  deriving (Generic, Show)

----------------------------------------------------------------------

instance (FromJSON s, FromJSON p) => FromJSON (Repo s p) where
  parseJSON = genericParseJSON $ unPrefix "_r_"

instance (ToJSON s, ToJSON p) => ToJSON (Repo s p) where
  toJSON     = genericToJSON     $ unPrefix "_r_"
  toEncoding = genericToEncoding $ unPrefix "_r_"

instance (Serialise s, Serialise p) => Serialise (Repo s p)

makeLenses ''Repo

initRepo :: Monoid s => Repo s p
initRepo = Repo 1 mempty []



--------------------

type RepoCmdM   env err m =
  ( CmdM'             env err m
  , HasConnectionPool env
  , HasConfig         env
  )


------------------------------------------------------------------------


-- Instances
instance Arbitrary NgramsRepoElement where
  arbitrary = elements $ map ngramsElementToRepo ns
    where
      NgramsTable ns = mockTable

instance FromHttpApiData (Map TableNgrams.NgramsType (Versioned NgramsTableMap))
  where
    parseUrlPiece x = maybeToEither x (decode $ cs x)

ngramsTypeFromTabType :: TabType -> TableNgrams.NgramsType
ngramsTypeFromTabType tabType =
  let here = "Garg.API.Ngrams: " :: Text in
    case tabType of
      Sources    -> TableNgrams.Sources
      Authors    -> TableNgrams.Authors
      Institutes -> TableNgrams.Institutes
      Terms      -> TableNgrams.NgramsTerms
      _          -> panic $ here <> "No Ngrams for this tab"
      -- TODO: This `panic` would disapear with custom NgramsType.

----
-- Async task

data UpdateTableNgramsCharts = UpdateTableNgramsCharts
  { _utn_tab_type :: !TabType
  , _utn_list_id  :: !ListId
  } deriving (Eq, Show, Generic)

makeLenses ''UpdateTableNgramsCharts
instance FromJSON UpdateTableNgramsCharts where
  parseJSON = genericParseJSON $ jsonOptions "_utn_"

instance ToJSON UpdateTableNgramsCharts where
  toJSON = genericToJSON $ jsonOptions "_utn_"

instance ToSchema UpdateTableNgramsCharts where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_utn_")

------------------------------------------------------------------------
type NgramsList = (Map TableNgrams.NgramsType (Versioned NgramsTableMap))

