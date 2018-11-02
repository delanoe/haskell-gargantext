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

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gargantext.API.Ngrams
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as DM
import Data.Map.Strict.Patch (Patch, apply, Edit, EditV, replace, transformWith, fromList)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics (Generic)

import Gargantext.Database.Ngram (NgramsId)
import Gargantext.Database.User  (UserId)
import Gargantext.Text.List.Types (ListType(..))
import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

data NgramsElement =
     NgramsElement { _ne_id     :: Int
                   , _ne_ngrams :: Text
                   , _ne_list   :: ListType
                   }
      deriving (Ord, Eq)
$(deriveJSON (unPrefix "_ne_") ''NgramsElement)


data NgramsTable = NgramsTable { _ngramsTable :: [Tree NgramsElement] }
  deriving (Ord, Eq, Generic)

instance ToJSON   NgramsTable
instance FromJSON NgramsTable

instance FromJSON (Tree NgramsElement)
-- TODO
instance ToJSON   (Tree NgramsElement)

--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data NgramsPatch =
     NgramsPatch { _np_list_types   :: Map UserId ListType
                 , _np_add_children :: Set NgramsId
                 , _np_rem_children :: Set NgramsId
                 }
      deriving (Ord, Eq, Show)

$(deriveJSON (unPrefix "_np_") ''NgramsPatch)

type NgramsIdPatch = Patch NgramsId NgramsPatch

------------------------------------------------------------------------
type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }


ngramsPatch :: NgramsPatch
ngramsPatch = NgramsPatch (DM.fromList [(1, StopList)]) Set.empty Set.empty

{-
toEdit :: NgramsId -> NgramsPatch -> Edit NgramsId NgramsPatch
toEdit n p = Edit n p
-}
ngramsIdPatch :: Patch NgramsId NgramsPatch
ngramsIdPatch = fromList $ catMaybes $ reverse [ replace (1::NgramsId) (Just ngramsPatch) Nothing
                                       , replace (1::NgramsId) Nothing (Just ngramsPatch)
                                       , replace (2::NgramsId) Nothing (Just ngramsPatch)
                                       ]


-- applyPatchBack :: Patch -> IO Patch
-- isEmptyPatch = Map.all (\x -> Set.isEmpty (add_children x) && Set.isEmpty ... )

------------------------------------------------------------------------
