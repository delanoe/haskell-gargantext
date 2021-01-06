{-|
Module      : Gargantext.Database.Types
Description : Specific Types to manage core Gargantext type with database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Types
  where

import Gargantext.Prelude
import Gargantext.Database.Schema.Prelude
import qualified Database.PostgreSQL.Simple as PGS


-- | Index memory of any type in Gargantext
type Index = Int
data Indexed a =
  Indexed { _unIndex   :: a
          , _index     :: Index
          }
  deriving (Show, Generic, Eq, Ord)

makeLenses ''Indexed

instance (FromField a) => PGS.FromRow (Indexed a) where
  fromRow = Indexed <$> field <*> field

