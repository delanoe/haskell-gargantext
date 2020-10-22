{-|
Module      : Gargantext.Core.Text.Corpus.API
Description : All crawlers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE InstanceSigs      #-}

module Gargantext.Core.Text.Corpus.API
  ( ExternalAPIs(..)
  , Query
  , Limit
  , get
  , externalAPIs
  )
    where

import Data.Maybe
import Gargantext.API.Admin.Orchestrator.Types (ExternalAPIs(..), externalAPIs)
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import qualified Gargantext.Core.Text.Corpus.API.Hal     as HAL
import qualified Gargantext.Core.Text.Corpus.API.Isidore as ISIDORE
import qualified Gargantext.Core.Text.Corpus.API.Istex   as ISTEX
import qualified Gargantext.Core.Text.Corpus.API.Pubmed  as PUBMED

-- | Get External API metadata main function
get :: ExternalAPIs
    -> Lang
    -> Query
    -> Maybe Limit
    -> IO [HyperdataDocument]
get PubMed  _la q l = PUBMED.get   q l -- EN only by default
get HAL      la q l = HAL.get   la q l
get IsTex    la q l = ISTEX.get la q l
get Isidore  la q l = ISIDORE.get la (fromIntegral <$> l) (Just q) Nothing
get _        _  _ _ = undefined

-- | Some Sugar for the documentation
type Query = PUBMED.Query
type Limit = PUBMED.Limit

