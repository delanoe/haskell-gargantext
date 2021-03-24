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

import Gargantext.Core.Types (Limit, Offset)
import Data.Maybe (Maybe, maybe)
import Opaleye (Query, limit, offset)

limit' ::  Maybe Limit -> Query a -> Query a
limit' maybeLimit query = maybe query (\l -> limit l query) maybeLimit

offset' :: Maybe Offset -> Query a  -> Query a
offset' maybeOffset query = maybe query (\o -> offset o query) maybeOffset


