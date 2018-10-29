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

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Core.Types (ListType(..))

data NgramsElement =
     NgramsElement { _nn_ngrams :: Text
                   , _nn_id     :: Int
                   , _nn_list   :: ListType
                   }
$(deriveJSON (unPrefix "_nn_") ''NgramsElement)


data NgramsTable = NgramsTable { _ngramsTable :: [Tree NgramsElement] }
  deriving (Generic)

instance ToJSON   NgramsTable
instance FromJSON NgramsTable

instance FromJSON (Tree NgramsElement)
-- TODO
instance ToJSON   (Tree NgramsElement)
-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
