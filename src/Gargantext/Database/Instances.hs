{-|
Module      : Gargantext.Database.Instances
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Gargantext.Database.Instances where

import Gargantext.Prelude

import Data.Text (Text)
import Data.Time (UTCTime)

import Opaleye (PGInt4, PGTimestamptz, PGFloat8
               , QueryRunnerColumnDefault
               , queryRunnerColumnDefault
               , fieldQueryRunnerColumn
               , Nullable, PGText)

instance QueryRunnerColumnDefault PGInt4 Integer where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGTimestamptz (Maybe UTCTime) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault (Nullable PGInt4) Int where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault (Nullable PGText) Text    where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

