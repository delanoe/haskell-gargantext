{-# LANGUAGE DerivingStrategies  #-}

module Gargantext.Core.Types.Query where

import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple.FromField as PSQL
import qualified Database.PostgreSQL.Simple.ToField as PSQL
import qualified Data.Swagger as Swagger
import Protolude
import qualified Servant.API as Servant


-- newtype wrappers based on
-- https://www.haskellforall.com/2023/04/ergonomic-newtypes-for-haskell-strings.html
-- These give stronger type guarantees than `type Offset = Int`.

-- Queries
newtype Limit = Limit { getLimit :: Int }
  deriving newtype ( Aeson.FromJSON, Aeson.ToJSON
                   , Eq, Num, Read, Show
                   , PSQL.FromField, PSQL.ToField
                   , Servant.FromHttpApiData, Servant.ToHttpApiData
                   , Swagger.ToParamSchema, Swagger.ToSchema)
newtype Offset = Offset { getOffset :: Int }
  deriving newtype ( Aeson.FromJSON, Aeson.ToJSON
                   , Eq, Num, Read, Show
                   , PSQL.FromField, PSQL.ToField
                   , Servant.FromHttpApiData, Servant.ToHttpApiData
                   , Swagger.ToParamSchema, Swagger.ToSchema)
type IsTrash  = Bool
