{-|
Module      : Gargantext.Database.Prelude
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}



module Gargantext.Database.Schema.Prelude
  ( module Control.Arrow
  , module Control.Lens.TH
  , module Data.Aeson.TH
  , module Data.Profunctor.Product.TH
  , module Data.Swagger
  , module Database.PostgreSQL.Simple.FromField
  , module Database.PostgreSQL.Simple.FromRow
  , module Database.PostgreSQL.Simple.SqlQQ
  , module Database.PostgreSQL.Simple.ToField
  , module Database.PostgreSQL.Simple.ToRow
  , module Database.PostgreSQL.Simple.Types
  , module GHC.Generics
  , module Gargantext.Core.Utils.Prefix
  , module Opaleye
  , module Opaleye.Internal.QueryArr
  , module Test.QuickCheck.Arbitrary
  )
  where

import Control.Arrow (returnA)
import Control.Lens.TH (makeLenses, makeLensesWith, abbreviatedFields)
import Data.Aeson.TH (deriveJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Swagger hiding (required, in_)
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Opaleye hiding (FromField, readOnly)
import Opaleye.Internal.QueryArr (Query)
import Test.QuickCheck.Arbitrary

import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ   (sql)
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Database.PostgreSQL.Simple.ToRow   (toRow)
import Database.PostgreSQL.Simple.Types   (Values(..), QualifiedIdentifier(..))

