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

-- import Gargantext.Database.User  (UserId)
import Data.Patch.Class (Replace, replace)
--import qualified Data.Map.Strict.Patch as PM
import Data.Monoid
--import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
--import Data.Maybe (catMaybes)
--import qualified Data.Map.Strict as DM
--import qualified Data.Set as Set
import Control.Lens (view, (.~))
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Either(Either(Left))
import Data.List (concat)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Swagger
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Gargantext.Core.Types (node_id)
--import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Ngrams (NgramsId)
import Gargantext.Database.Node (getListsWithParentId)
import Gargantext.Database.NodeNgram -- (NodeNgram(..), NodeNgram, updateNodeNgrams, NodeNgramPoly)
import Gargantext.Database.NodeNgramsNgrams -- (NodeNgramsNgramsPoly(NodeNgramsNgrams))
import Gargantext.Prelude
import Gargantext.Text.List.Types (ListType(..), ListId, ListTypeId) -- ,listTypeId )
import Prelude (Enum, Bounded, minBound, maxBound)
import Servant hiding (Patch)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------
--data FacetFormat = Table | Chart
data TabType   = Docs   | Terms  | Sources | Authors | Institutes | Trash
  deriving (Generic, Enum, Bounded)

instance FromHttpApiData TabType
  where
    parseUrlPiece "Docs"       = pure Docs
    parseUrlPiece "Terms"      = pure Terms
    parseUrlPiece "Sources"    = pure Sources
    parseUrlPiece "Institutes" = pure Institutes
    parseUrlPiece "Authors"    = pure Authors
    parseUrlPiece "Trash"      = pure Trash
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
  deriving (Ord, Eq, Generic, ToJSON, FromJSON)

instance Arbitrary NgramsTable where
  arbitrary = elements
              [ NgramsTable
                [ NgramsElement "animal"  GraphList     1  Nothing       (Set.fromList ["dog", "cat"])
                , NgramsElement "cat"     GraphList     1 (Just "animal") mempty
                , NgramsElement "dog"     GraphList     3 (Just "animal")(Set.fromList ["dogs"])
                , NgramsElement "dogs"    StopList      4 (Just "dog")    mempty
                , NgramsElement "object"  CandidateList 2  Nothing        mempty
                , NgramsElement "nothing" StopList      4  Nothing        mempty
                ]
              , NgramsTable
                [ NgramsElement "organic" GraphList     3  Nothing        (Set.singleton "flower")
                , NgramsElement "flower"  GraphList     3 (Just "organic") mempty
                , NgramsElement "moon"    CandidateList 1  Nothing         mempty
                , NgramsElement "cat"     GraphList     2  Nothing         mempty
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

data NgramsIdPatch =
     NgramsIdPatch { _nip_ngrams      :: NgramsTerm
                   , _nip_ngramsPatch :: NgramsPatch
                   }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_nip_") ''NgramsIdPatch)

instance ToSchema  NgramsIdPatch

instance Arbitrary NgramsIdPatch where
  arbitrary = NgramsIdPatch <$> arbitrary <*> arbitrary

                       --
-- TODO:
-- * This should be a Map NgramsId NgramsPatch
-- * Patchs -> Patches
newtype NgramsIdPatchs =
     NgramsIdPatchs { _nip_ngramsIdPatchs :: [NgramsIdPatch] }
      deriving (Ord, Eq, Show, Generic, Arbitrary)
$(deriveJSON (unPrefix "_nip_") ''NgramsIdPatchs)
instance ToSchema  NgramsIdPatchs

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
type CorpusId = Int

type TableNgramsApiGet = Summary " Table Ngrams API Get"
                      :> QueryParam "ngramsType"   TabType
                      :> QueryParam "list"   ListId
                      :> Get    '[JSON] NgramsTable

type TableNgramsApi = Summary " Table Ngrams API Change"
                      :> QueryParam "list"   ListId
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

{-
toLists :: ListId -> NgramsIdPatchs -> [(ListId, NgramsId, ListTypeId)]
-- toLists = undefined
toLists lId np = [ (lId,ngId,listTypeId lt) | map (toList lId) (_nip_ngramsIdPatchs np) ]

toList :: ListId -> NgramsIdPatch -> (ListId, NgramsId, ListTypeId)
toList = undefined

toGroups :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsIdPatchs -> [NodeNgramsNgrams]
toGroups lId addOrRem ps = concat $ map (toGroup lId addOrRem) $ _nip_ngramsIdPatchs ps

toGroup :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsIdPatch -> [NodeNgramsNgrams]
-- toGroup = undefined
toGroup lId addOrRem (NgramsIdPatch ngId patch)  =
  map (\ng -> (NodeNgramsNgrams lId ngId ng (Just 1))) (Set.toList $ addOrRem patch)

-}

tableNgramsPatch :: Connection -> CorpusId -> Maybe ListId -> NgramsIdPatchsFeed -> IO NgramsIdPatchsBack
tableNgramsPatch = undefined 
{-
tableNgramsPatch conn corpusId maybeList patchs = do
  listId <- case maybeList of
              Nothing      -> defaultList conn corpusId
              Just listId' -> pure listId'
  _ <- ngramsGroup' conn Add $ toGroups listId _np_add_children patchs
  _ <- ngramsGroup' conn Del $ toGroups listId _np_rem_children patchs
  _ <- updateNodeNgrams conn (toLists listId patchs)
  pure (NgramsIdPatchs [])
-}

getTableNgramsPatch :: Connection -> CorpusId -> Maybe TabType -> Maybe ListId -> IO NgramsTable
getTableNgramsPatch = undefined
