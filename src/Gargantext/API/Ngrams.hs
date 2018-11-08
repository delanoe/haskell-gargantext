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
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}

module Gargantext.API.Ngrams
  where

-- import Gargantext.Database.User  (UserId)
--import Data.Map.Strict.Patch (Patch, replace, fromList)
--import Data.Maybe (catMaybes)
--import qualified Data.Map.Strict as DM
--import qualified Data.Set as Set
import Control.Lens (view)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Either(Either(Left))
import Data.List (concat)
import Data.Set (Set)
import Data.Swagger (ToSchema, ToParamSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Gargantext.Core.Types (node_id)
import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Ngrams (NgramsId)
import Gargantext.Database.Node (getListsWithParentId)
-- import Gargantext.Database.NodeNgram -- (NodeNgram(..), NodeNgram, updateNodeNgrams, NodeNgramPoly)
import Gargantext.Database.NodeNgramsNgrams -- (NodeNgramsNgramsPoly(NodeNgramsNgrams))
import Gargantext.Prelude
import Gargantext.Text.List.Types (ListType(..), ListId, ListTypeId) --,listTypeId )
import Prelude (Enum, Bounded, minBound, maxBound)
import Servant hiding (Patch)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
-- import qualified Data.Set as Set

------------------------------------------------------------------------
--data FacetFormat = Table | Chart
data TabType   = Docs   | Terms  | Sources | Authors | Trash
  deriving (Generic, Enum, Bounded)

instance FromHttpApiData TabType
  where
    parseUrlPiece "Docs"    = pure Docs
    parseUrlPiece "Terms"   = pure Terms
    parseUrlPiece "Sources" = pure Sources
    parseUrlPiece "Authors" = pure Authors
    parseUrlPiece "Trash"   = pure Trash
    parseUrlPiece _         = Left "Unexpected value of TabType"

instance ToParamSchema   TabType
instance ToJSON    TabType
instance FromJSON  TabType
instance ToSchema  TabType
instance Arbitrary TabType
  where
    arbitrary = elements [minBound .. maxBound]

------------------------------------------------------------------------
data NgramsElement =
     NgramsElement { _ne_ngrams      :: Text
                   , _ne_list        :: ListType
                   , _ne_occurrences :: Int
                   }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_ne_") ''NgramsElement)

instance ToSchema NgramsElement
instance Arbitrary NgramsElement where
  arbitrary = elements [NgramsElement "sport" StopList 1]

------------------------------------------------------------------------
data NgramsTable = NgramsTable { _ngramsTable :: [Tree NgramsElement] }
  deriving (Ord, Eq, Generic)
$(deriveJSON (unPrefix "_") ''NgramsTable)

instance Arbitrary NgramsTable where
  arbitrary = NgramsTable <$> arbitrary

-- TODO
instance Arbitrary (Tree NgramsElement) where 
  arbitrary = elements [ TreeN (NgramsElement "animal" GraphList 1) 
                            [TreeN (NgramsElement "dog" GraphList 3) []
                             , TreeN (NgramsElement "object" CandidateList 2) []
                             , TreeN (NgramsElement "cat"     GraphList 1) []
                             , TreeN (NgramsElement "nothing" StopList 4) []
                            ]
                       , TreeN (NgramsElement "plant" GraphList 3)
                            [TreeN (NgramsElement "flower" GraphList 3) []
                             , TreeN (NgramsElement "moon" CandidateList 1) []
                             , TreeN (NgramsElement "cat"     GraphList 2) []
                             , TreeN (NgramsElement "sky" StopList 1) []
                            ]
                       ]
instance ToSchema NgramsTable

------------------------------------------------------------------------
-- On the Client side:
--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data NgramsPatch =
     NgramsPatch { _np_list_types   :: ListType   -- TODO Map UserId ListType
                 , _np_add_children :: Set NgramsElement
                 , _np_rem_children :: Set NgramsElement
                 }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_np_") ''NgramsPatch)

instance ToSchema  NgramsPatch

instance Arbitrary NgramsPatch where
  arbitrary = NgramsPatch <$> arbitrary <*> arbitrary <*> arbitrary

                       --

data NgramsIdPatch = 
     NgramsIdPatch { _nip_ngramsId    :: NgramsElement
                   , _nip_ngramsPatch :: NgramsPatch
                   }
      deriving (Ord, Eq, Show, Generic)

$(deriveJSON (unPrefix "_nip_") ''NgramsIdPatch)

instance ToSchema  NgramsIdPatch

instance Arbitrary NgramsIdPatch where
  arbitrary = NgramsIdPatch <$> arbitrary <*> arbitrary

                       --

data NgramsIdPatchs =
     NgramsIdPatchs { _nip_ngramsIdPatchs :: [NgramsIdPatch] }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_nip_") ''NgramsIdPatchs)
instance ToSchema  NgramsIdPatchs
instance Arbitrary NgramsIdPatchs where
  arbitrary = NgramsIdPatchs <$> arbitrary

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
type TableNgramsApi = Summary " Table Ngrams API Change"
                      :> QueryParam "list"   ListId
                      :> ReqBody '[JSON] NgramsIdPatchs
                      :> Put     '[JSON] NgramsIdPatchsBack

type TableNgramsApiGet = Summary " Table Ngrams API Get"
                      :> QueryParam "ngramsType"   TabType
                      :> QueryParam "list"   ListId
                      :> Get    '[JSON] NgramsTable

type NgramsIdPatchsFeed = NgramsIdPatchs
type NgramsIdPatchsBack = NgramsIdPatchs


defaultList :: Connection -> CorpusId -> IO ListId
defaultList c cId = view node_id <$> maybe (panic errorMessage) identity 
  <$> head 
  <$> getListsWithParentId c cId
  where
    errorMessage = "Gargantext.API.Ngrams.defaultList: no list found"

toLists :: ListId -> NgramsIdPatchs -> [(ListId, NgramsId, ListTypeId)]
toLists lId np = map (toList lId) (_nip_ngramsIdPatchs np)

toList :: ListId -> NgramsIdPatch -> (ListId, NgramsId, ListTypeId)
toList = undefined
-- toList lId (NgramsIdPatch ngId (NgramsPatch lt _ _)) = (lId,ngId,listTypeId lt)

toGroups :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsIdPatchs -> [NodeNgramsNgrams]
toGroups lId addOrRem ps = concat $ map (toGroup lId addOrRem) $ _nip_ngramsIdPatchs ps

toGroup :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsIdPatch -> [NodeNgramsNgrams]
toGroup = undefined
{-
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
