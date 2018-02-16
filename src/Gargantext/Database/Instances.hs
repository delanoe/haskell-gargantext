{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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


