{-|
Module      : Gargantext.Core.Text.Corpus.API
Description : All crawlers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Core.Text.Corpus.API
  ( ExternalAPIs(..)
  , Query
  , Limit
  , get
  , externalAPIs
  ) where

import Data.Maybe
import Gargantext.API.Admin.Orchestrator.Types (ExternalAPIs(..), externalAPIs)
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import qualified Gargantext.Core.Text.Corpus.API.Hal     as HAL
import qualified Gargantext.Core.Text.Corpus.API.Isidore as ISIDORE
import qualified Gargantext.Core.Text.Corpus.API.Istex   as ISTEX
import qualified Gargantext.Core.Text.Corpus.API.Pubmed  as PUBMED

-- | TODO put in gargantext.init
default_limit :: Maybe Integer
default_limit = Just 10000

-- | Get External API metadata main function
get :: ExternalAPIs
    -> Lang
    -> Query
    -> Maybe Limit
    -> IO [HyperdataDocument]
get PubMed  _la q _l = PUBMED.get   q default_limit -- EN only by default
get HAL      la q _l = HAL.get   la q default_limit
get IsTex    la q _l = ISTEX.get la q default_limit
get Isidore  la q _l = ISIDORE.get la (fromIntegral <$> default_limit) (Just q) Nothing
get _        _  _ _  = undefined

-- | Some Sugar for the documentation
type Query = PUBMED.Query
type Limit = PUBMED.Limit
