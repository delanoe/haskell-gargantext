{-|
Module      : Gargantext.API.Types
Description : Server API main Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RankNTypes         #-}

module Gargantext.API.Types
  where

import Servant
import Gargantext.API.Settings
import Gargantext.API.Ngrams
import Gargantext.Database.Tree
import Gargantext.Core.Types
import Gargantext.Database.Utils
import Gargantext.Database.Schema.Node


type GargServer api =
  forall env err m.
    ( CmdM env err m
    , HasNodeError err
    , HasInvalidError err
    , HasTreeError err
    , HasRepo env
    , HasSettings env
    )
    => ServerT api m




