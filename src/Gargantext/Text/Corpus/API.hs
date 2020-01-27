{-|
Module      : Gargantext.Text.Corpus.API
Description : All crawlers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}

module Gargantext.Text.Corpus.API
  ( ExternalAPIs(..)
  , Query
  , Limit
  , get
  , externalAPIs
  )
    where

import Data.Maybe
import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.API.Orchestrator.Types (ExternalAPIs(..), externalAPIs)
import Gargantext.Database.Types.Node (HyperdataDocument(..))

import qualified Gargantext.Text.Corpus.API.Pubmed  as PUBMED
import qualified Gargantext.Text.Corpus.API.Isidore as ISIDORE
import qualified Gargantext.Text.Corpus.API.Hal     as HAL
import qualified Gargantext.Text.Corpus.API.Istex   as ISTEX

-- | Get External API metadata main function
get :: ExternalAPIs -> Query -> Maybe Limit -> IO [HyperdataDocument]

get PubMed     q l = PUBMED.get q l

get HAL_EN     q l = HAL.get EN q l
get HAL_FR     q l = HAL.get FR q l

get IsTex_EN   q l = ISTEX.get EN q l
get IsTex_FR   q l = ISTEX.get FR q l

get Isidore_EN q l = ISIDORE.get EN (fromIntegral <$> l) (Just q) Nothing
get Isidore_FR q l = ISIDORE.get FR (fromIntegral <$> l) (Just q) Nothing

get _  _ _ = undefined
-- | Some Sugar for the documentation
type Query = PUBMED.Query
type Limit = PUBMED.Limit

