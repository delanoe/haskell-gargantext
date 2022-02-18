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
  )
    where

import Conduit
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
    -- -> IO [HyperdataDocument]
    -> IO (ConduitT () HyperdataDocument IO ())
get PubMed  _la q _l = do
  res <- PUBMED.get   q default_limit -- EN only by default
  pure $ yieldMany res
get HAL      la q _l = HAL.getC  la q Nothing
get IsTex    la q _l = do
  res <- ISTEX.get la q default_limit
  pure $ yieldMany res
get Isidore  la q _l = do
  res <- ISIDORE.get la (fromIntegral <$> default_limit) (Just q) Nothing
  pure $ yieldMany res
get _        _  _ _ = undefined

-- | Some Sugar for the documentation
type Query = PUBMED.Query
type Limit = PUBMED.Limit
