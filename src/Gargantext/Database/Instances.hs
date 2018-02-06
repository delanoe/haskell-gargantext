{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.Database.Instances where

import Gargantext.Prelude

import Data.Time (UTCTime)
import Opaleye (PGInt4, PGTimestamptz, PGFloat8
               , QueryRunnerColumnDefault
               , queryRunnerColumnDefault
               , fieldQueryRunnerColumn
               )

instance QueryRunnerColumnDefault PGInt4 Integer where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGFloat8 (Maybe Double) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGTimestamptz (Maybe UTCTime) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn


