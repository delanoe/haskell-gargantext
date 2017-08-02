{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Gargantext.Database.Instances where

import Data.Time (UTCTime)
import Opaleye (Column, PGBool, PGInt4, PGText, PGTimestamptz, PGFloat8
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




