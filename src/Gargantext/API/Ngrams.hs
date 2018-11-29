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
-- get data of NgramsTable
-- post :: update NodeNodeNgrams
-- group ngrams

get ngrams filtered by NgramsType
add get 

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}

module Gargantext.API.Ngrams
  where

import Prelude (round)
-- import Gargantext.Database.Schema.User  (UserId)
import Data.Patch.Class (Replace, replace)
--import qualified Data.Map.Strict.Patch as PM
import Data.Monoid
--import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
--import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as DM
import Data.Map.Strict (Map)
--import qualified Data.Set as Set
import Control.Lens ((.~))
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Either(Either(Left))
import Data.Map (lookup)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Swagger
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
--import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Types.Node (NodeType(..))
import Gargantext.Database.Schema.Node (defaultList)
import qualified Gargantext.Database.Schema.Ngrams as Ngrams
import Gargantext.Prelude
import Gargantext.Core.Types (ListType(..), ListId, CorpusId)
import Prelude (Enum, Bounded, minBound, maxBound)
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
$(deriveJSON (unPrefix "_ne_") ''NgramsElement)

instance ToSchema NgramsElement
instance Arbitrary NgramsElement where
  arbitrary = elements [NgramsElement "sport" GraphList 1 Nothing mempty]

------------------------------------------------------------------------
newtype NgramsTable = NgramsTable { _ngramsTable :: [NgramsElement] }
  deriving (Ord, Eq, Generic, ToJSON, FromJSON, Show)

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
-- On the Client side:
--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data PatchSet a = PatchSet
  { _rem :: Set a
  , _add :: Set a
  }
  deriving (Eq, Ord, Show, Generic)

instance (Ord a, Arbitrary a) => Arbitrary (PatchSet a) where
  arbitrary = PatchSet <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (PatchSet a) where
  toJSON     = genericToJSON     $ unPrefix "_"
  toEncoding = genericToEncoding $ unPrefix "_"

instance (Ord a, FromJSON a) => FromJSON (PatchSet a) where
  parseJSON = genericParseJSON $ unPrefix "_"

instance ToSchema a => ToSchema (PatchSet a)

instance ToSchema a => ToSchema (Replace a) where
  declareNamedSchema (_ :: proxy (Replace a)) = do
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
     NgramsPatch { _patch_children :: PatchSet NgramsElement
                 , _patch_list     :: Replace ListType   -- TODO Map UserId ListType
                 }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_") ''NgramsPatch)

-- instance Semigroup NgramsPatch where

instance ToSchema  NgramsPatch

instance Arbitrary NgramsPatch where
  arbitrary = NgramsPatch <$> arbitrary <*> (replace <$> arbitrary <*> arbitrary)

-- TODO:
-- * This should be a Map NgramsId NgramsPatch
-- * Patchs -> Patches
newtype NgramsTablePatch =
     NgramsTablePatch { _nip_ngramsIdPatchs :: Map NgramsTerm NgramsPatch }
      deriving (Ord, Eq, Show, Generic, Arbitrary)
$(deriveJSON (unPrefix "_nip_") ''NgramsTablePatch)
instance ToSchema  NgramsTablePatch

------------------------------------------------------------------------
------------------------------------------------------------------------
type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }

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
                      :> Get    '[JSON] NgramsTable

type TableNgramsApi = Summary " Table Ngrams API Change"
                      :> QueryParam "list"   ListId
<<<<<<< HEAD
                      :> ReqBody '[JSON] NgramsIdPatchsFeed -- Versioned ...
                      :> Put     '[JSON] NgramsIdPatchsBack -- Versioned ...

type NgramsIdPatchsFeed = NgramsIdPatchs
type NgramsIdPatchsBack = NgramsIdPatchs


||||||| parent of 06bfb6e... WIP
                      :> ReqBody '[JSON] NgramsIdPatchsFeed -- Versioned ...
                      :> Put     '[JSON] NgramsIdPatchsBack -- Versioned ...

type NgramsIdPatchsFeed = NgramsIdPatchs
type NgramsIdPatchsBack = NgramsIdPatchs


defaultList :: Connection -> CorpusId -> IO ListId
defaultList c cId = view node_id <$> maybe (panic noListFound) identity 
  <$> head
  <$> getListsWithParentId c cId
  where
    noListFound = "Gargantext.API.Ngrams.defaultList: no list found"

=======
                      :> ReqBody '[JSON] (Versioned NgramsTablePatch)
                      :> Put     '[JSON] (Versioned NgramsTablePatch)

data NgramError = NoListFound
  deriving (Show)

class HasNgramError e where
  _NgramError :: Prism' e NgramError

instance HasNgramError ServantErr where
  _NgramError = prism' mk (const Nothing) -- Note a prism
    where
      mk NoListFound = err404 { errBody = "NgramError: No list found"           }
      mk EmptyRoot    = err500 { errBody = "Root node should not be empty" }
      mk TooManyRoots = err500 { errBody = "Too many root nodes"           }

ngramError :: (MonadError e m, HasNgramError e) => NgramError -> m a
ngramError nne = throwError $ _NgramError # nne

defaultList :: ( MonadError e m
               , HasNgramError e
               , MonadReader env m
               , HasConnection env
               ) => CorpusId -> m ListId
defaultList cId = view node_id =<< maybe (ngramError NoListFound) pure
  <$> head
  <$> getListsWithParentId cId

>>>>>>> 06bfb6e... WIP
{-
toLists :: ListId -> NgramsTablePatch -> [(ListId, NgramsId, ListTypeId)]
-- toLists = undefined
toLists lId np = [ (lId,ngId,listTypeId lt) | map (toList lId) (_nip_ngramsIdPatchs np) ]

toList :: ListId -> NgramsIdPatch -> (ListId, NgramsId, ListTypeId)
toList = undefined

toGroups :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsTablePatch -> [NodeNgramsNgrams]
toGroups lId addOrRem ps = concat $ map (toGroup lId addOrRem) $ _nip_ngramsIdPatchs ps

toGroup :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsIdPatch -> [NodeNgramsNgrams]
-- toGroup = undefined
toGroup lId addOrRem (NgramsIdPatch ngId patch)  =
  map (\ng -> (NodeNgramsNgrams lId ngId ng (Just 1))) (Set.toList $ addOrRem patch)

-}

-- Apply the given patch to the DB and returns the patch to be applied on the
-- cilent.
-- TODO:
-- In this perliminary version the OT aspect is missing, therefore the version
-- number is always 1 and the returned patch is always empty.
tableNgramsPatch :: ( MonadError e m
                    , HasNgramError e
                    , MonadReader env m
                    , HasConnection env
                    , MonadIO m
                    )
                 => CorpusId -> Maybe ListId
                 -> Versioned NgramsTablePatch
                 -> m (Versioned NgramsTablePatch)
tableNgramsPatch conn corpusId maybeList (Versioned version patch) = do
  when (version /= 1) $ ngramError $ UnsupportedVersion v
  listId <- maybe defaultList pure maybeList
{-
  _ <- ngramsGroup' conn Add $ toGroups listId _np_add_children patch
  _ <- ngramsGroup' conn Del $ toGroups listId _np_rem_children patch
  _ <- updateNodeNgrams conn (toLists listId patch)
-}
  pure $ Version 1 mempty

-- | TODO Errors management
--  TODO: polymorphic for Annuaire or Corpus or ...
-- getTableNgrams :: Connection -> CorpusId -> Maybe TabType -> Maybe ListId -> IO NgramsTable
getTableNgrams :: Connection -> CorpusId -> Handler TableNgramsApiGet
getTableNgrams c cId maybeTabType maybeListId = do
  let lieu = "Garg.API.Ngrams: " :: Text
  let ngramsType = case maybeTabType of
        Nothing  -> Ngrams.Sources -- panic (lieu <> "Indicate the Table")
        Just tab -> case tab of
            Sources    -> Ngrams.Sources
            Authors    -> Ngrams.Authors
            Institutes -> Ngrams.Institutes
            Terms      -> Ngrams.NgramsTerms
            _          -> panic $ lieu <> "No Ngrams for this tab"

  listId <- case maybeListId of
      Nothing -> defaultList c cId
      Just lId -> pure lId

  (ngramsTableDatas, mapToParent, mapToChildren) <-
    Ngrams.getNgramsTableDb c NodeDocument ngramsType (Ngrams.NgramsTableParam listId cId)

  -- printDebug "ngramsTableDatas" ngramsTableDatas

  pure $ NgramsTable $ map (\(Ngrams.NgramsTableData ngs _ lt w) ->
                              NgramsElement ngs
                                            (maybe (panic $ lieu <> "listType") identity lt)
                                            (round w)
                                            (lookup ngs mapToParent)
                                            (maybe mempty identity $ lookup ngs mapToChildren)
                           ) ngramsTableDatas


