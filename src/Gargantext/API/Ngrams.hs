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

import Prelude (Enum, Bounded, minBound, maxBound)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either(Either(Left))
import Data.Aeson.TH (deriveJSON)
import Database.PostgreSQL.Simple (Connection)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
--import qualified Data.Map.Strict as DM
--import Data.Map.Strict.Patch (Patch, replace, fromList)
import Data.Text (Text)
--import Data.Maybe (catMaybes)
import Data.Set (Set)
--import qualified Data.Set as Set

import GHC.Generics (Generic)

import Gargantext.Database.Ngram (NgramsId)
import Gargantext.Database.NodeNgram (updateNodeNgrams)
import Gargantext.Database.User  (UserId)
import Gargantext.Text.List.Types (ListType(..))
import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude
import Servant hiding (Patch)

import Data.Swagger (ToSchema, ToParamSchema)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

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
     NgramsElement { _ne_id     :: Int
                   , _ne_ngrams :: Text
                   , _ne_list   :: ListType
                   }
      deriving (Ord, Eq)
$(deriveJSON (unPrefix "_ne_") ''NgramsElement)


data NgramsTable = NgramsTable { _ngramsTable :: [Tree NgramsElement] }
  deriving (Ord, Eq, Generic)
$(deriveJSON (unPrefix "_") ''NgramsTable)


-- TODO
instance FromJSON (Tree NgramsElement)
instance ToJSON   (Tree NgramsElement)

------------------------------------------------------------------------
-- On the Client side:
--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data NgramsPatch =
     NgramsPatch { _np_list_types   :: Map UserId ListType
                 , _np_add_children :: Set NgramsId
                 , _np_rem_children :: Set NgramsId
                 }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_np_") ''NgramsPatch)

instance ToSchema  NgramsPatch

instance Arbitrary NgramsPatch where
  arbitrary = NgramsPatch <$> arbitrary <*> arbitrary <*> arbitrary

                       --

data NgramsIdPatch = 
     NgramsIdPatch { _nip_ngramsId    :: NgramsId
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
type ListId   = Int
type TableNgramsApi = Summary " Table Ngrams API"
                      :> QueryParam "list"   ListId
                      :> ReqBody '[JSON] NgramsIdPatchs
                      :> Put     '[JSON] NgramsIdPatchsBack

type NgramsIdPatchsFeed = NgramsIdPatchs
type NgramsIdPatchsBack = NgramsIdPatchs


getDefaultList :: Connection -> CorpusId -> IO ListId
getDefaultList = undefined

type NgramsIdParent = Int
type NgramsIdChild  = Int

data Action = Del | Add


doNgramsGroup :: Connection -> ListId -> Action -> [(NgramsIdParent, NgramsIdChild)] -> IO [Int] 
doNgramsGroup = undefined


tableNgramsPatch :: Connection -> CorpusId -> Maybe ListId -> NgramsIdPatchsFeed -> IO NgramsIdPatchsBack
tableNgramsPatch conn corpusId maybeList patchs = do
  listId <- case maybeList of
              Nothing      -> getDefaultList conn corpusId
              Just listId' -> pure listId'
  --_ <- doNgramsGroups conn listId Add $ 
  --_ <- delNgramsGroups conn listId
  --_ <- updateNodeNgrams   conn 
  pure (NgramsIdPatchs [])


