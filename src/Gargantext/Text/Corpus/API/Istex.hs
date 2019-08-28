{-|
Module      : Gargantext.Text.Corpus.API.Istex
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Corpus.API.Istex
    where

import Data.Maybe
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import qualified Data.Text as Text


import qualified ISTEX        as ISTEX
import qualified ISTEX.Client as ISTEX


get :: Lang -> Text -> Maybe Integer -> IO [HyperdataDocument]
get la q ml = undefined
