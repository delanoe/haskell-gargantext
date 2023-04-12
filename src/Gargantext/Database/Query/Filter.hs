{-|
Module      : Gargantext.Database.Query.Filter
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}



{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Gargantext.Database.Query.Filter
  where

import Gargantext.Core.Types.Query (Limit(..), Offset(..))
import Data.Maybe (Maybe, maybe)
import Opaleye (Select, limit, offset)

limit' ::  Maybe Limit -> Select a -> Select a
limit' maybeLimit query = maybe query (\l -> limit (getLimit l) query) maybeLimit

offset' :: Maybe Offset -> Select a  -> Select a
offset' maybeOffset query = maybe query (\o -> offset (getOffset o) query) maybeOffset
