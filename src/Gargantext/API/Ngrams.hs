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
import Data.Map.Strict.Patch (Patch, apply, transformWith)
import Data.Text (Text)
import Data.Set (Set)
import GHC.Generics (Generic)

import Gargantext.Database.Ngram (NgramsId)
import Gargantext.Database.User  (UserId)
import Gargantext.Core.Types (ListType(..))
import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Prelude

data NgramsElement =
     NgramsElement { _ne_id     :: Int
                   , _ne_ngrams :: Text
                   , _ne_list   :: ListType
                   }
$(deriveJSON (unPrefix "_ne_") ''NgramsElement)


data NgramsTable = NgramsTable { _ngramsTable :: [Tree NgramsElement] }
  deriving (Generic)

instance ToJSON   NgramsTable
instance FromJSON NgramsTable

instance FromJSON (Tree NgramsElement)
-- TODO
instance ToJSON   (Tree NgramsElement)

--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data NgramsPatch =
     NgramsPatch { list_types   :: Map UserId ListType
                 , add_children :: Set NgramsId
                 , rem_children :: Set NgramsId
                 }

data Patch = Map NgramsId NgramsPatch

-- applyPatchBack :: Patch -> IO Patch
-- isEmptyPatch = Map.all (\x -> Set.isEmpty (add_children x) && Set.isEmpty ... )

-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
