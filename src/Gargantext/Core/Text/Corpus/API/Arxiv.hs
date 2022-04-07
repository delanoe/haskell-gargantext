{-|
Module      : Gargantext.Core.Text.Corpus.API.Arxiv
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}

module Gargantext.Core.Text.Corpus.API.Arxiv
    where

import Conduit
import Data.Either (Either(..))
import Data.Maybe
import Data.Text (Text)
--import qualified Data.Text as Text
import Servant.Client (ClientError)

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))

--import qualified Arxiv.Wrapper as Arxiv


type Query = Text
type Limit = Int

-- | TODO put default pubmed query in gargantext.ini
-- by default: 10K docs
get :: Lang -> Query -> Maybe Limit -> IO (Either ClientError (Maybe Integer, ConduitT () HyperdataDocument IO ()))
get _la _q _l = pure $ Right $ (Nothing, yieldMany [])
