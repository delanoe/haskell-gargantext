{-|
Module      : Gargantext.Text.Corpus.API.Hal
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Corpus.API.Hal
    where

import Data.Maybe
import Data.Text (Text)
import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Types.Node (HyperdataDocument(..))
import qualified Data.Text as Text

import qualified HAL     as HAL
import qualified HAL.Doc.Corpus as HAL

get :: Lang -> Text -> Maybe Integer -> IO [HyperdataDocument]
get la q li = undefined

